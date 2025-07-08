/*
 * Harbour MiniGUI Extended - TagBox Control
 * -----------------------------------------
 * This example demonstrates a TagBox control implementation 
 * using a Textbox for input and a Listbox for displaying tags.
 * 
 * Features:
 * Users can enter tags in the Textbox and press Enter to add them.
 * Duplicate tags are automatically prevented.
 * Tags are displayed in a Listbox.
 * Users can remove a selected tag by clicking the "Remove Tag" button.
 * Supports CTRL+RETURN as an additional shortcut to add tags.
 * 
 * How It Works:
 * AddTag(): Adds a tag when the user presses Enter or CTRL+RETURN.
 *  - The tag is trimmed and checked for duplicates.
 *  - If valid, it is added to the Listbox.
 * RemoveTag(): Removes the selected tag from the Listbox.
 *  - Ensures that the selection remains valid after removal.
 * The Listbox keeps track of all entered tags.
 * The Textbox is cleared after adding a tag.
 * 
 * Advantages:
 * Provides an easy way to manage multiple tags.
 * Simple, clean, and user-friendly.
 * Uses built-in MiniGUI controls for efficiency.
 * Supports keyboard shortcuts for faster interaction.
 *
 * Developed for Harbour MiniGUI Extended.
 * Ideal for category management, keyword tagging, or search filters.
 */

/*
 * How to test this example:
 * - User adds a tag: The tag appears in lstTags (Search Filters).
 * - Results update dynamically: lstResults displays only matching items.
 * - User removes a tag: The list updates accordingly.
 * Search is case-insensitive and matches partial words.
 *
 * Expected Behavior:
 * Type "app" and press Enter: Filters "Apple".
 * Type "pe" and press Enter: Filters "Peach", "Grape".
 * Remove a filter: Updates list accordingly.
 */

#include "minigui.ch"

STATIC aData := { "Apple", "Banana", "Orange", "Grape", "Mango", "Peach", "Cherry", "Strawberry" }
STATIC aTags := {} // Search filters (tags)

FUNCTION Main()
    DEFINE WINDOW Win_1 ;
        AT 100, 100 ;
        WIDTH 500 ;
        HEIGHT 450 ;
        TITLE "TagBox Search Filter Example" ;
        MAIN

        @ 20, 20 LABEL lblTag VALUE "Enter Filter:" WIDTH 100 HEIGHT 20
        @ 20, 120 TEXTBOX txtTag WIDTH 200 HEIGHT 25 TOOLTIP "Type a tag and press Enter" ON ENTER AddTag()
        @ 50, 120 LISTBOX lstTags WIDTH 200 HEIGHT 100 ITEMS {} TOOLTIP "Search Filters"
        @ 160, 120 BUTTON btnRemove CAPTION "Remove Tag" ACTION RemoveTag() WIDTH 100 HEIGHT 30 TOOLTIP "Remove selected tag"

        @ 220, 20 LABEL lblResults VALUE "Results:" WIDTH 100 HEIGHT 20
        @ 240, 120 LISTBOX lstResults WIDTH 200 HEIGHT 150 ITEMS aData TOOLTIP "Filtered Items"

        ON KEY CTRL+RETURN ACTION AddTag()

    END WINDOW

    ACTIVATE WINDOW Win_1
RETURN NIL

FUNCTION AddTag()
    LOCAL cTag := AllTrim(Win_1.txtTag.Value)

    IF !Empty(cTag) .AND. aScan(aTags, {|x| x == cTag}) == 0
        AADD(aTags, cTag)
        Win_1.lstTags.SetArray(aTags)
        Win_1.lstTags.Value := Win_1.lstTags.ItemCount
        UpdateResults()
    ENDIF

    Win_1.txtTag.Value := ""  // Clear input after any attempt
RETURN NIL

FUNCTION RemoveTag()
    LOCAL nIndex := Win_1.lstTags.Value

    IF nIndex > 0 .AND. nIndex <= LEN(aTags)
        ADEL(aTags, nIndex, .T.)
        Win_1.lstTags.SetArray(aTags)
        IF nIndex > LEN(aTags)
            Win_1.lstTags.Value := Win_1.lstTags.ItemCount
        ENDIF
        UpdateResults()
    ENDIF
RETURN NIL

FUNCTION UpdateResults()
    LOCAL aFiltered := {}
    LOCAL i, cItem

    // Apply search filters
    IF Empty(aTags)
        aFiltered := aData
    ELSE
        FOR EACH cItem IN aData
            FOR i := 1 TO LEN(aTags)
                IF Lower(aTags[i]) $ Lower(cItem) // Check if item matches any tag
                    AADD(aFiltered, cItem)
                    EXIT
                ENDIF
            NEXT
        NEXT
    ENDIF

    Win_1.lstResults.SetArray(aFiltered)
RETURN NIL
