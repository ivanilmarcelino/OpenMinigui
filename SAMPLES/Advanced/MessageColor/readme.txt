Hello everyone,
I wanted to share some code which, although not something really useful or indispensable, 
makes it possible to make a message more pleasing to the eye.

Using as a starting point the source "MINIGUI - SAMPLES - BASIC - COLORED_LABEL.PRG" 
I wrote a function to display a simple message box or a or a message with choice options 
in which it is possible to change the properties (FontName, FontSize, FontColor, BackColor, 
FontBold, FontItalic, FontUnderline, Tooltip, On MouseHover) of a single word (or several 
words) or a single character (or several characters) contained within the message window 
(it is also possible to change the properties of choice buttons).

Basically, I modified the cLbl class in order to extend the customisable properties:
- The original class accepts a word (or words) to be customised and, when processing, 
  customises all occurrences of those words in the message in case they occur multiple times.
- My modified class accepts the position of the words or individual characters to be 
  customised and, when processing, customises only the specified words or characters 
  (regardless of the number of occurrences).

I also enclose a main.prg source with some tests.

Paolino Rappa
2023-10-30


Syntax
    MyMsgCustom([sMessage], [sTitle], [nRowWnd], [nColWnd], [sWndIcon], [aBtns], [sAlertIcon], [aBtnBColor],
               [aLblProperty], [aLblOpt]) -> nChoice

   Parameters:
    sMessage
       Character type message to display. It can be split across multiple lines using the function
       HB_EOL(). DEFAULT -> "" (no message)
       Example: sMessage := "First line message" + HB_EOL() + "Second line message"
   
    sTitle
       Window header. DEFAULT -> ""
   
    nRowWnd,nColWnd
       Coordinates of the upper left corner of the window.
       It is possible to make the window look like this:
       - Centered with respect to the DESKTOP, passing the value -1 to the nRowWnd parameter
       - Centered with respect to the calling window, passing the value -2 to the nRowWnd parameter
       Instead of the values -1 and -2 it is possible to use the corresponding constants _MSG_CENTER_IN_DESKTOP and
       _MSG_CENTER_IN_PARENT present in the MYMESSAGE.CH file.
       The DEFAULT value of nRowWnd is -1 (window centered with respect to the DESKTOP) and that of nColWnd is 0
       (however if the value of nRowWnd is -1 or -2 the value of nColWnd is not taken into account)
      
    sWndIcon
       Icon to display in the window header. The image file name must be complete
       of PATH and extension. DEFAULT -> NIL (no image)
   
    aBtns
       Matrix containing the parameters of the choice options to be selected. DEFAULT -> NIL (Window with
       message only, without buttons).
       aButtons format:
        - aButtons[01] -> Button number selected for DEFAULT.
        - aButtons[02] -> Array containing the formatting of the options with the following format:
          - Array[01] -> Name of the FONT to use
          - Array[02] -> FONT size
          - Array[03] -> Foreground color of the FONT
          - Array[04] -> FONT background color
          - Array[05] -> Logical value for bold
          - Array[06] -> Logical value for the italic style
          - Array[07] -> Logical value for underline
          DEFAULT -> {"Arial",10,Black,White,.F.,.F.,.F.}
        - aButtons[03] -> Value to display (CAPTION property).
        - aButtons[04] -> Position of the character contained in aButtons[03] to be used as a shortcut from
                          keyboard to select the button. A value of 0 indicates that it should not be
                          no letters highlighted. DEFAULT -> see __CtrlKeys function
        - aButtons[05] -> Image to display (CAPTION property). The image file name must
                          be complete with PATH and extension. DEFAULT -> NIL (no image)
        - aButtons[06] -> Value to assign to the TOOLTIP property of the button. DEFAULT -> ""
       Example aBtns := {1,{"Arial",10,Black,White,.F.,.F.,.F.},{"Ok",1,"_Ok.png",""}}
   
    sAlertIcon
       Image to display inside the window. The image file name must be complete with
       PATH and extension. DEFAULT -> NIL (no image)
   
    aBtnBColor
       Background color of the window in the button area. In case of presence of buttons the window
       visually it is divided into two parts: the first contains the image and the text of the message (in
       in this case the color corresponds to the background color of the message), the second contains the buttons
       of the selectable options (aBtnBColor). DEFAULT -> Standard background color of the window

    aLblProperty
       Array containing the DEFAULT values to use for displaying the message. These values
       correspond to:
        - Name of the FONT to use
        - FONT size
        - Foreground color of the FONT
        - FONT background color
        - Logical value for bold
        - Logical value for the Italic style
        - Logical value for underlining
       DEFAULT -> aLblProperty := {"Arial",10,COLOR_Black,COLOR_White,.F.,.F.,.F.}
   
    aLblOpt
       Matrix containing:
        - as the first element a logical value indicating whether the text of the message must be modified a
          word (.T.) or character (.F.) level. It is possible to use constant equivalents
          _MSG_WORD_SELECTION and _MSG_CHAR_SELECTION present in the MYMESSAGE.CH file.
        - as subsequent elements an array containing the following values:
          - Position of the word or character to modify within the message.
            ATTENTION: any carriage returns (HB_EOL()) are not considered. This aspect is
            very important in case the selection is made by character (_MSG_CHAR_SELECTION).
            Suppose, for example, that the message is "Test for" + HB_EOL() + "test" the letter 'p' from the
            word 'test' will be character number 8 (as if the text were "Test test"
          - Name of the FONT to use for the character or word specified with the previous value
          - FONT size. DEFAULT -> Value corresponding to that of the aLblProperty parameter
          - Foreground color of the FONT. DEFAULT -> Value corresponding to that of the parameter
            aLblProperty
          - FONT background color. DEFAULT -> Value corresponding to that of the aLblProperty parameter
          - Logical value for bold. DEFAULT -> Value corresponding to that of the aLblProperty parameter
          - Logical value for the Italic style. DEFAULT -> Value corresponding to that of the parameter
            aLblProperty
          - Logical value for underlining. DEFAULT -> Value corresponding to that of the parameter
            aLblProperty
          - Text to display when the mouse passes over the indicated character or word (TOOLTIP).
            DEFAULT -> NIL (no text)
          - Logical value to change or not the shape of the cursor in one hand when hovering the mouse over
            character or word indicated. DEFAULT -> .F.
          - Block of code to execute by clicking on the indicated character or word. DEFAULT -> NIL
       the DEFAULT value of aLblOpt is {_MSG_WORD_SELECTION). This means that a will be displayed
       message with uniform text.

   Returns:
    nChoice
       The position number of the selected option.

   Description
    MyMsgColor() displays a window containing a text message and to which you can add
    a series of options to select (such as ALERT()).
    The peculiarity of the function is that the content of the message can be modified at the level
    character or word. The possible changes concern the type of FONT, its size, its color
    foreground, its background color, the use or not of bold, the use or not of style
    italic, the use or not of underlining, the possibility of displaying a text when passing
    mouse on the desired element, the possibility of changing the shape of the mouse icon into that of a hand
    on mouse hover and execution of a block of code on left mouse click.
