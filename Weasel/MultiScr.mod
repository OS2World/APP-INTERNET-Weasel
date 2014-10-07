(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2014   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE MultiScreen;

        (****************************************************************)
        (*                                                              *)
        (*  This module allows the creation of multiple virtual screens *)
        (*  - complete screens, not just windows - with three keyboard  *)
        (*      "hot keys" used to navigate around the screens.         *)
        (*                                                              *)
        (*  We support a two-level hierarchy: a virtual screen is a     *)
        (*  collection of windows, and each virtual screen is a member  *)
        (*  of a group.  (In addition, there is a pseudo-group made up  *)
        (*  of all windows which are not associated with any virtual    *)
        (*  screen, to handle the "normal output" which bypasses this   *)
        (*  module.)  The virtual screen switching is done using hot    *)
        (*  keys, as follows:                                           *)
        (*     Hotkey1   steps to the previous group                    *)
        (*     Hotkey2   steps to the next group                        *)
        (*     Hotkey3   steps to the next virtual screen of the        *)
        (*               currently active group.                        *)
        (*                                                              *)
        (*  In the present version, the client module has to call       *)
        (*  EnableHotKeys to define the three hot keys.                 *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        22 December 2013                        *)
        (*  Status:             Mostly working                          *)
        (*      Still don't properly support duplicating the same       *)
        (*      window on multiple virtual screens.  At this stage      *)
        (*      I'm not convinced that this would be worth the trouble. *)
        (*                                                              *)
        (****************************************************************)

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM Keyboard IMPORT
    (* proc *)  HotKey;

FROM Windows IMPORT
    (* type *)  Window, DisplayPage,
    (* proc *)  Hide, PutOnTop, PutOnPage, SetActivePage, InstallCloseHandler;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)

TYPE
    (* A WindowRecord identifies one window. *)

    WRpointer = POINTER TO WindowRecord;
    WindowRecord =  RECORD
                        next: WRpointer;
                        window: Window;
                    END (*RECORD*);

    (* A WindowList is a linear list of all the windows which have      *)
    (* been associated with a given virtual screen.                     *)

    WindowList = RECORD
                     head, tail: WRpointer;
                 END (*RECORD*);

    (* Each group of virtual screens is arranged as a circular list.    *)
    (* Each virtual screen has an associated list of windows.           *)

    VirtualScreen = POINTER TO VirtualScreenRecord;
    VirtualScreenRecord = RECORD
                              next: VirtualScreen;
                              group: ScreenGroup;
                              windowlist: WindowList;
                          END (*RECORD*);

    (* The groups are also arranged as a circular list.  There is       *)
    (* one special "sentinel" group, called NormalOutput, which refers  *)
    (* to normal non-virtual screen output.                             *)
    (* The currentVS field selects the virtual screen which is at       *)
    (* present the active virtual screen in this group, and - for ease  *)
    (* of insertion - the preVS field identifies the predecessor of     *)
    (* currentVS in the circular list.                                  *)

    ScreenGroup = POINTER TO ScreenGroupRecord;
    ScreenGroupRecord = RECORD
                            previous, next: ScreenGroup;
                            page: DisplayPage;
                            preVS, currentVS: VirtualScreen;
                        END (*RECORD*);

(************************************************************************)

VAR
    (* The wakeup semaphores are signalled (by the keyboard module)     *)
    (* whenever the hot keys are typed.  We interpret this as a command *)
    (* to switch to the previous/next group, or the next virtual screen *)
    (* within the current group, depending on which hot key was typed.  *)

    wakeup1, wakeup2, wakeup3: Semaphore;

    (* ActiveGroup is the currently active group, and NormalOutput is a *)
    (* pointer to a sentinel record in the master list of all groups.   *)
    (* The condition ActiveGroup = NormalOutput is interpreted to mean  *)
    (* that no group should be active, i.e. the only windows displayed  *)
    (* are those not associated with any virtual screen.                *)

    ActiveGroup, NormalOutput: ScreenGroup;

    (* We use a single lock to protect references to the entire group   *)
    (* structure, including references to ActiveGroup and Occupant.     *)
    (* The alternative, of separate protection for the various          *)
    (* substructures, turns out to create potential deadlock problems,  *)
    (* whose solution becomes messy.                                    *)
    (* Remark: I'd rather use a Lock rather than a semaphore here.      *)
    (* However this will have to wait until I can remove semaphore      *)
    (* waits from procedures Hide and PutOnTop (in module Windows).     *)

    guard: Semaphore;

    (* Occupant[p] is the virtual screen currently occupying hardware   *)
    (* page p.  We use this in our Hide/Unhide decisions.  Note that    *)
    (* windows not belonging to any virtual screen will not show up in  *)
    (* the Occupant information.                                        *)

    Occupant: ARRAY DisplayPage OF VirtualScreen;

    (* ScreenChangeEnabled controls whether screen switching is         *)
    (* allowed.  While this is FALSE, the hot keys are ignored.         *)

    ScreenChangeEnabled: BOOLEAN;

(************************************************************************)
(*              REMOVING A WINDOW FROM THE GROUP STRUCTURE              *)
(************************************************************************)

PROCEDURE RemoveFromVirtualScreen (w: Window;  VS: VirtualScreen);

    (* Removes window w from virtual screen VS (without closing it)     *)
    (* if it is found there.                                            *)

    VAR previous, current: WRpointer;

    BEGIN
        previous := NIL;  current := VS^.windowlist.head;
        LOOP
            IF current = NIL THEN
                EXIT (*LOOP*);
            ELSIF current^.window = w THEN
                IF previous = NIL THEN
                    VS^.windowlist.head := current^.next;
                ELSE
                    previous^.next := current^.next;
                END (*IF*);
                IF VS^.windowlist.tail = current THEN
                    VS^.windowlist.tail := previous;
                END (*IF*);
                DISPOSE (current);
                EXIT (*LOOP*);
            ELSE
                previous := current;  current := current^.next;
            END (*IF*);
        END (*LOOP*);
    END RemoveFromVirtualScreen;

(************************************************************************)

PROCEDURE RemoveFromGroup (w: Window;  group: ScreenGroup);

    (* See description of RemoveWindow, below.  *)

    VAR current: VirtualScreen;

    BEGIN
        WITH group^ DO
            current := currentVS;
            IF current <> NIL THEN
                REPEAT
                    RemoveFromVirtualScreen (w, current);
                    current := current^.next;
                UNTIL current = currentVS;
            END (*IF*);
        END (*WITH*);
    END RemoveFromGroup;

(************************************************************************)

PROCEDURE RemoveWindow (w: Window;  page: DisplayPage);

    (* Removes the association, if any, between window w and the        *)
    (* virtual screen(s) with which it is associated.  It is possible   *)
    (* that no such association exists, in which case we do nothing.    *)

    VAR current: ScreenGroup;

    BEGIN
        Wait (guard);
        current := NormalOutput^.next;
        WHILE current <> NormalOutput DO
            IF current^.page = page THEN
                RemoveFromGroup (w, current);
            END (*IF*);
            current := current^.next;
        END (*WHILE*);
        Signal (guard);
    END RemoveWindow;

(************************************************************************)
(*                          FINDING A WINDOW                            *)
(************************************************************************)

PROCEDURE WindowIsInScreen (w: Window;  VS: VirtualScreen): BOOLEAN;

    (* Returns TRUE iff w is a window in VS. *)

    VAR current: WRpointer;

    BEGIN
        current := VS^.windowlist.head;
        LOOP
            IF current = NIL THEN
                RETURN FALSE;
            END (*IF*);
            IF current^.window = w THEN
                RETURN TRUE;
            END (*IF*);
            current := current^.next;
        END (*LOOP*);
    END WindowIsInScreen;

(************************************************************************)

PROCEDURE FindInGroup (w: Window;  G: ScreenGroup): VirtualScreen;

    (* Like VirtualScreenOf (see below), except that the caller         *)
    (* specifies which group to search.                                 *)

    VAR result: VirtualScreen;

    BEGIN
        result := G^.currentVS;
        WHILE (result <> NIL) AND NOT WindowIsInScreen(w, result) DO
            IF result = G^.preVS THEN
                result := NIL;
            ELSE
                result := result^.next;
            END (*IF*);
        END (*WHILE*);
        RETURN result;
    END FindInGroup;

(************************************************************************)

PROCEDURE VirtualScreenOf (w: Window): VirtualScreen;

    (* Returns the virtual screen, if any, with which window w is       *)
    (* associated.  It is possible that no such association exists, in  *)
    (* which case we return a NIL result.                               *)

    VAR current: ScreenGroup;  result: VirtualScreen;

    BEGIN
        result := NIL;
        Wait (guard);
        current := NormalOutput^.next;
        LOOP
            IF current = NormalOutput THEN EXIT(*LOOP*) END(*IF*);
            result := FindInGroup (w, current);
            IF result <> NIL THEN EXIT(*LOOP*) END(*IF*);
            current := current^.next;
        END (*LOOP*);
        Signal (guard);
        RETURN result;
    END VirtualScreenOf;

(************************************************************************)
(*                  HIDING AND UNHIDING SCREEN OUTPUT                   *)
(************************************************************************)

PROCEDURE HideAll (MP: VirtualScreen);

    (* Makes all windows in this virtual screen invisible.      *)

    VAR list: WRpointer;

    BEGIN
        IF MP <> NIL THEN
            list := MP^.windowlist.head;
            WHILE list <> NIL DO
                Hide (list^.window);
                list := list^.next;
            END (*WHILE*);
        END (*IF*);
    END HideAll;

(************************************************************************)

PROCEDURE UnhideAll (MP: VirtualScreen);

    (* Makes all windows in this virtual screen visible.        *)

    VAR list: WRpointer;

    BEGIN
        IF MP <> NIL THEN
            list := MP^.windowlist.head;
            WHILE list <> NIL DO
                PutOnTop (list^.window);
                list := list^.next;
            END (*WHILE*);
        END (*IF*);
    END UnhideAll;

(************************************************************************)
(*                      TASKS TO HANDLE KEYBOARD INPUT                  *)
(************************************************************************)

PROCEDURE StepToGroup (newgroup: ScreenGroup);

    BEGIN
        WITH newgroup^ DO
            IF page <> ActiveGroup^.page THEN
                SetActivePage (page);
            END (*IF*);
            IF Occupant[page] <> currentVS THEN
                HideAll (Occupant[page]);
                UnhideAll (currentVS);
                Occupant[page] := currentVS;
            END (*IF*);
        END (*WITH*);
        ActiveGroup := newgroup;
    END StepToGroup;

(************************************************************************)

PROCEDURE HotKeyHandler1;

    (* Each time Hotkey1 is typed, this task switches to the previous   *)
    (* group of virtual pages provided that ScreenChangeEnabled is set. *)

    BEGIN
        LOOP    (* forever *)
            Wait (wakeup1);
            Wait (guard);
            IF ScreenChangeEnabled THEN
                StepToGroup (ActiveGroup^.previous);
            END (*IF*);
            Signal (guard);
        END (*LOOP*);
    END HotKeyHandler1;

(************************************************************************)

PROCEDURE HotKeyHandler2;

    (* Each time Hotkey2 is typed, this task switches to the next       *)
    (* group of virtual pages provided that ScreenChangeEnabled is set. *)

    BEGIN
        LOOP    (* forever *)
            Wait (wakeup2);
            Wait (guard);
            IF ScreenChangeEnabled THEN
                StepToGroup (ActiveGroup^.next);
            END (*IF*);
            Signal (guard);
        END (*LOOP*);
    END HotKeyHandler2;

(************************************************************************)

PROCEDURE HotKeyHandler3;

    (* Each time Hotkey3 is typed, this task switches to the next       *)
    (* virtual page within the current group provided that              *)
    (* ScreenChangeEnabled is set.                                      *)

    BEGIN
        LOOP    (* forever *)
            Wait (wakeup3);
            Wait (guard);
            IF ScreenChangeEnabled THEN
                WITH ActiveGroup^ DO
                    IF (currentVS <> NIL)
                                 AND (currentVS^.next <> currentVS) THEN
                        preVS := currentVS;  currentVS := currentVS^.next;
                        HideAll (preVS);
                        UnhideAll (currentVS);
                        Occupant[page] := currentVS;
                    END (*IF*);
                END (*WITH*);
            END (*IF*);
            Signal (guard);
        END (*LOOP*);
    END HotKeyHandler3;

(************************************************************************)
(*                      EXTERNALLY CALLABLE PROCEDURES                  *)
(************************************************************************)

PROCEDURE EnableScreenSwitching (enable: BOOLEAN);

    (* Enables hot key screen switching if enable is TRUE, or disables  *)
    (* it if enable is FALSE.                                           *)

    BEGIN
        ScreenChangeEnabled := enable;
    END EnableScreenSwitching;

(************************************************************************)

PROCEDURE EnableHotKeys (flag1: BOOLEAN;  key1: CHAR;
                         flag2: BOOLEAN;  key2: CHAR;
                         flag3: BOOLEAN;  key3: CHAR);

    (* Creates the "hot key" tasks, and enable the hot keys.  The first *)
    (* and second pair of parameters define the previous/next group     *)
    (* switching hot keys, and the third pair defines the hot key for   *)
    (* cycling within a group.                                          *)
    (* The "flag" part of each parameter pair is FALSE for a normal     *)
    (* key, and TRUE for an extended key defined by a two-character     *)
    (* sequence (where the first character is CHR(0).                   *)

    BEGIN
        HotKey (flag1, key1, wakeup1);
        EVAL(CreateTask (HotKeyHandler1, 9, "Group prev"));
        HotKey (flag2, key2, wakeup2);
        EVAL(CreateTask (HotKeyHandler2, 9, "Group next"));
        HotKey (flag3, key3, wakeup3);
        EVAL(CreateTask (HotKeyHandler3, 9, "Cycle hot key"));
    END EnableHotKeys;

(************************************************************************)

PROCEDURE CreateScreenGroup (hardwarepage: DisplayPage): ScreenGroup;

    (* Creates a new screen group, and maps it to the specified display *)
    (* page in the screen hardware.  It is permissible to map more than *)
    (* one group to the same hardware page.  Note that putting a group  *)
    (* on hardware page 0 means that it shares the screen with the      *)
    (* "normal output" which does not belong to any virtual page.  This *)
    (* is permitted, but usually inadvisable on aesthetic grounds.      *)

    VAR result: ScreenGroup;

    BEGIN
        NEW (result);
        WITH result^ DO
            previous := NIL;  next := NIL;
            page := hardwarepage;  currentVS := NIL;  preVS := NIL;
        END (*WITH*);

        (* Insert the new result into the circular list of groups.      *)
        (* In the present version, we insert it just ahead of the       *)
        (* special group NormalOutput, which gives the effect of        *)
        (* putting the groups in order of creation.                     *)

        Wait (guard);
        NormalOutput^.previous^.next := result;
        result^.previous := NormalOutput^.previous;
        result^.next := NormalOutput;
        NormalOutput^.previous := result;
        Signal (guard);

        RETURN result;

    END CreateScreenGroup;

(************************************************************************)

PROCEDURE CreateVirtualScreen (group: ScreenGroup): VirtualScreen;

    (* Adds a new virtual screen to the specified group.        *)

    VAR result: VirtualScreen;

    BEGIN
        NEW (result);
        WITH result^ DO
            next := NIL;  windowlist.head := NIL;  windowlist.tail := NIL;
        END (*WITH*);
        result^.group := group;

        (* Insert the new result into the list of virtual screens       *)
        (* belonging to the specified group.  It's inserted just before *)
        (* the current virtual screen for this group; in most cases     *)
        (* this means that virtual screens are ordered according to the *)
        (* order of their creation.                                     *)

        Wait (guard);
        WITH group^ DO
            IF currentVS = NIL THEN
                currentVS := result;
                preVS := result;
                result^.next := result;
            ELSE
                result^.next := currentVS;
                preVS^.next := result;
                preVS := result;
            END (*IF*);
        END (*WITH*);
        Signal (guard);

        RETURN result;

    END CreateVirtualScreen;

(************************************************************************)

PROCEDURE MapToVirtualScreen (w: Window;  screen: VirtualScreen);

    (* Before calling this procedure, both w and screen must have been  *)
    (* created.  This procedure ensures that window w is visible on     *)
    (* the screen only when the given virtual screen page is active.    *)
    (* The association lasts until the window is closed or the virtual  *)
    (* screen is removed.                                               *)

    VAR p: WRpointer;

    BEGIN
        IF NOT WindowIsInScreen (w, screen) THEN
            NEW (p);
            WITH p^ DO
                next := NIL;  window := w;
            END (*WITH*);

            Wait (guard);

            WITH screen^ DO
                WITH windowlist DO
                    IF head = NIL THEN
                        head := p;
                    ELSE
                        tail^.next := p;
                    END (*IF*);
                    tail := p;
                END (*WITH*);
                PutOnPage (w, group^.page);
            END (*WITH*);

            (* Hide or display the new window as appropriate.  *)

            WITH screen^.group^ DO
                IF Occupant[page] = NIL THEN
                    Occupant[page] := screen;
                END (*IF*);
                IF Occupant[page] = screen THEN
                    PutOnTop (w);
                ELSE
                    Hide(w);
                END (*IF*);
            END (*WITH*);

            InstallCloseHandler (w, RemoveWindow);
            Signal (guard);

        END (*IF*);

    END MapToVirtualScreen;

(************************************************************************)
(*              REMOVING VIRTUAL SCREENS AND SCREEN GROUPS              *)
(************************************************************************)

PROCEDURE RemoveVirtualScreen (VAR (*INOUT*) screen: VirtualScreen);

    (* Destroys all associations between the given virtual screen and   *)
    (* its windows (but does not close the windows), and permanently    *)
    (* removes this screen from the collection of virtual screens.      *)

    VAR wptr, wnext: WRpointer;
        VSptr: VirtualScreen;

    BEGIN
        Wait (guard);

        (* Detach the VirtualScreenRecord from its group.  This might   *)
        (* require updating preVS and currentVS for the group.  Beware  *)
        (* of the special case of a group with only one element.        *)

        WITH screen^.group^ DO
            IF currentVS = preVS THEN
                preVS := NIL;  currentVS := NIL;
            ELSIF screen = currentVS THEN
                currentVS := screen^.next;
                preVS^.next := currentVS;
            ELSE
                (* Let VSptr point to the screen ahead of this one.     *)
                VSptr := currentVS;
                WHILE VSptr^.next <> screen DO
                    VSptr := VSptr^.next;
                END(*WHILE*);
                IF screen = preVS THEN
                    preVS := VSptr;
                END (*IF*);
                VSptr^.next := screen^.next;
            END (*IF*);

            (* Deal with the special case of a virtual screen which is  *)
            (* now the occupant of its hardware page - i.e. it is now   *)
            (* visible, or will become visible as soon as that hardware *)
            (* page is activated.                                       *)

            IF screen = Occupant[page] THEN
                HideAll (screen);
                UnhideAll (currentVS);
                Occupant[page] := currentVS;
            END (*IF*);
        END (*WITH*);

        (* Discard the windowlist for this screen.      *)

        wptr := screen^.windowlist.head;
        WHILE wptr <> NIL DO
            wnext := wptr^.next;
            DISPOSE (wptr);
            wptr := wnext;
        END (*WHILE*);

        DISPOSE (screen);
        Signal (guard);

    END RemoveVirtualScreen;

(************************************************************************)

PROCEDURE RemoveScreenGroup (VAR (*INOUT*) group: ScreenGroup);

    (* As above, but removes an entire group.   *)

    BEGIN
        (* Start by emptying the group.  We can cut down the overhead   *)
        (* a little by leaving group^.currentVS until last.             *)

        WITH group^ DO
            WHILE currentVS <> NIL DO
                RemoveVirtualScreen (currentVS^.next);
            END (*WHILE*);
        END (*WITH*);

        (* Detach this group from the circular list of all groups.      *)

        Wait (guard);
        IF ActiveGroup = group THEN
            StepToGroup (group^.next);
        END (*IF*);
        WITH group^ DO
            previous^.next := next;
            next^.previous := previous;
        END (*WITH*);
        Signal (guard);

        DISPOSE (group);

    END RemoveScreenGroup;

(************************************************************************)
(*                         MODULE INITIALISATION                        *)
(************************************************************************)

VAR p: DisplayPage;

BEGIN
    (* There are initially no virtual screens.  *)

    FOR p := 0 TO MAX(DisplayPage) DO
        Occupant[p] := NIL;
    END (*FOR*);

    (* Create the global semaphores.    *)

    CreateSemaphore (wakeup1, 0);
    CreateSemaphore (wakeup2, 0);
    CreateSemaphore (wakeup3, 0);
    CreateSemaphore (guard, 1);

    (* Create the initial list of groups: a circular list with one      *)
    (* element, that element being the special sentinel which refers to *)
    (* normal screen output rather than one of the created groups.      *)

    Wait (guard);
    NEW (ActiveGroup);
    WITH ActiveGroup^ DO
        previous := ActiveGroup;  next := ActiveGroup;
        preVS := NIL;  currentVS := NIL;  page := 0;
    END (*WITH*);
    NormalOutput := ActiveGroup;
    ScreenChangeEnabled := TRUE;
    Signal (guard);

END MultiScreen.

