# ABAP-Check-Translations
Report to check the translations elements in ABAP

## Instalation:

1. Clone the repo
2. Create the ZCAPRR0000 program with the SE38 transaction
3. Copy the source code in each file into SAP
4. Copy the selection screen text elements
5. Activate

## How to use it:

1. Run the ZCAPRR0000 program
2. Enter the object name to analize
3. Choose the destination translate language (you can only show the installed languagues)
4. Choose the object type
5. Press `ENTER`and then `F8` (run)

## The Icons:

| Color  | Meaning |
|--------|---------|
| RED    | Not Translated |
| YELLOW | Translated but not confirmed |
| GREEN  | Translated and confirmed |

## Screenshots

Selection-Screen
<br />
![Selection-Screen](/images/selection_screen.png)

Not Translated
<br />
![Not Translated](/images/not_translated.png)

Translated
<br />
![Translated](/images/translated.png)
