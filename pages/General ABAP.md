<!-- title: ABAP Development Notes -->
<script src="https://jeremyc2.github.io/CDN/sync-favicon-with-title.js"></script>
<script src="https://jeremyc2.github.io/CDN/markdown-themes.js"></script>

# SAP Tables

<details open><summary>Table of Contents</summary>

- [SAP Tables](#sap-tables)
  - [General Notes](#general-notes)
    - [Data and Parameters Statements](#data-and-parameters-statements)
    - [Field Symbols](#field-symbols)
    - [Assignment Statements](#assignment-statements)
    - [String Special Comparison Operators](#string-special-comparison-operators)
    - [Loops](#loops)
  - [T-Codes](#t-codes)
    - [Standard T-Codes](#standard-t-codes)
    - [Special T-Codes](#special-t-codes)
  - [My Local Development Objects](#my-local-development-objects)
  - [ABAP CRUD](#abap-crud)
  - [Internal Tables](#internal-tables)
    - [The Append Statement](#the-append-statement)
    - [Reading from Internal Table](#reading-from-internal-table)
  - [ABAP Code Snippets](#abap-code-snippets)
    - [Hello World](#hello-world)
    - [Get Last Day of Previous Month](#get-last-day-of-previous-month)
    - [Explicitly typed declaration](#explicitly-typed-declaration)
    - [Implicit inline declaration (Since ABAP 7.40)](#implicit-inline-declaration-since-abap-740)
    - [Define Internal tables](#define-internal-tables)
    - [Field Symbols](#field-symbols-1)
    - [Create Internal Table Using Values Construct](#create-internal-table-using-values-construct)
    - [Case Statement](#case-statement)
    - [Loops](#loops-1)
  - [Cool Things](#cool-things)
  - [TODO List](#todo-list)
  - [VS Code Markdown Help](#vs-code-markdown-help)
    - [Extensions](#extensions)
    - [Tips and Shortcuts](#tips-and-shortcuts)
</details>

## General Notes

T-Code ABAPDOCU - ABAP Documentation ([Available Online][ABAP-Docu])

ABAP is interpreted.  
ABAP is [Whitespace sensitive][5].  
ABAP is not case-sensitive.  
Statements end with a period.  
Remember [Chained Statements][4]  
**$TMP** package is non-transportable  

Sample Programs: 
  * Package **SABAPDEMOS** (from **SE38**/**SE80** or **SE16** with table **TADIR** ([also here](Demo-Programs-and-Templates.md)))
  * **BC\*ALV\*** or **BC\*ALV\*DEMO** (i.e. **BCALV_GRID_05** (see [**ALV** programming][10]))

Program Search:
  1. **SE38**
  2. **F4** in the program field
  3. Click **System Information**
  4. Click **All Selections**

UI Example:
  * Transaction **BIBS**
  * Program **SAPLEXAMPLE_ENTRY_SCREEN**

RICEF:
  * Reports - Show Data
  * Interfaces - Connection from one system to another (i.e. Coupa to SAP)
  * Conversions - Load data from non-SAP to SAP (i.e. from Excel)
  * Enhancements - Enhance the standard logic without disturbing the standard User Exits
  * Forms - SAP Script, Smartforms, Adobe froms -> all used to generate output (i.e. PDF)
  * Workflow - Tool for automating the business process

Comments:
  * \* Makes the entire line a comment
  * " Makes the rest of the line a comment

Executable program types:
  1. Reports: Accepts parameters from the user and produces an interactive list
  2. Module pools: More complex user interaction using a collection of screens.

Non-executable program types:
  1. INCLUDE modules: Used to subdivide large programs
  2. Subroutine pools: Contains ABAP subroutines (blocks of code enclosed by FORM/ENDFORM statements and invoked with PERFORM)
  3. Function groups: Contains function modules (enclosed by FUNCTION/ENDFUNCTION and invoked with CALL FUNCTION)
  4. Object classes & Interfaces: Object Oriented Programming
  5. Type pools: Collections of data types and constants

Data Dictionary:
  1. Tables: Relational. 
     1. Transparent: 1-to-1 relationship between the definition of a table in the ABAP Dictionary and the definition of that same table in the database (same name, same columns).
     2. Pooled: Separate tables in Data Dictionary, but grouped together at the database level
     3. Clustered: Grouped based on their primary keys
  2. Indexes: Speeds up database access for highly-frequented objects
  3. Views: Virual table
  4. Structures: Similar to C++ struct. Can also represent a single row in a table
  5. Data elements: Data type
  6. Domains: Characteristics of data elements. Can also contain possible values
  7. Search helps: List of possible values for a field
  8. Lock objects: Controls access, prevents race conditions

### Data and Parameters Statements

```
data v1[(1)] [type t] [decimals d] [value 'xxx']  
```

```
data v1 like v2 [value 'xxx']  
```

</br>

```
 parameters p1[(1)] [type t] [decimals d] [default 'xxx'] [obligatory] [lower case] [as checkbox] [radiobutton group g]  
 ```

 ```
parameters p1 like v1 [default 'xxx'] [obligatory] [lower case] [as checkbox] [radiobutton group g]  
```

*You can change the parameter input field labels via the menu path Goto->Text Elements.*

<details closed>
<summary>Parameter Tips</summary>  

Always use the like addition to define a parameter. When you use it, the parameter acquires the following attributes from the Data Dictionary:  
* F1 help is acquired from the Documentation button in the data element.  
* F4 help is acquired if your parameter is like one that has a check table.  
* A field label obtained from the Data Dictionary is guaranteed to be consistent with the field label presented by other programs for the same field (provided they also obtain them from the DDIC). This eliminates the confusion of having two fields that are labeled differently refer to the same data.  
* Modifications to the data type or length in the DDIC are automatically reflected by your program.  
  
In view of all of these advantages, you should always use the like addition to define a parameter. This applies even to check boxes and radio buttons. If necessary, you should create a structure in the DDIC so that you can use like and at a minimum, provide F1 help. Note that F1 help is available even for radio buttons and check boxes.
</details>
</br>

Built-in Data Types:
  * I: Integer
  * P: Packed decimal
  * F: Floating point
  * N: Character numeric
  * C: Character
  * D: Date
  * T: Time
  * X: Hexadecimal
  * STRING: Variable-length string
  * XSTRING: Variable-length raw byte array

> Hex values and strings (if they are being compared with another string) must be in all-caps  
> Escape single quotes in strings with a single quote  
> You may have to include single quotes around some other data types  

| Data Type | Internal Description | Default Internal Length | Max Internal Length | Valid Values | Default Initial Value |
| --------- | -------------------- | ----------------------- | ------------------- | ------------ | --------------------- |
| c         | character            | 1                       | 65535               | Any Char     | Blank                 |
| n         | numeric text         | 1                       | 65535               | 0-9          | 0                     |
| d         | date                 | 8 (fixed)               | -                   | 0-9          | 00000000              |
| t         | time                 | 6 (fixed)               | -                   | 0-9          | 00000000              |
| x         | hexadecimal          | 1                       | 65535               | Any          |                       |

| Data Type | Description    | Default Internal Length | Max Length | Max Decimals      | Valid Values    | Default Initial Value |
| --------- | -------------- | ----------------------- | ---------- | ----------------- | --------------- | --------------------- |
| i         | integer        | 4(fixed)                | -          | 0                 | -231 to +231    | 0                     |
| p         | packed decimal | 8                       | 16         | 14                | 0-9             | 0                     |
| f         | floating-point | 8                       | 8          | 15 (Also depends) | -1E307 to 1E308 | 0.0                   |

* **Date**
  * **Format:** YYYYMMDD
  * **Current Date:** sydatum
* **Time**
  * **Format:** HHMMSS
  * **Current Time:** sy-uzeis

> The values of sy-datum and sy-uzeit are set at the beginning of program execution
and do not change until the program ends. If you need access to the most current date and
time during execution of a long-running pro-gram, use the statement get time. It updates
the values of sy-datum and sy-uzeit to reflect the current date and time.

Message types:
  * A: Abend
  * E: Error
  * I: Information
  * S: Success/Status
  * W: Warning
  * X: Exit (Terminates and logs info to T-Code ST22)

### Field Symbols

A **field-symbol** is a pointer you can dynamically assign
to a field. After assignment, you can use the field-symbol
anywhere in your program in place of the actual field
name. Use the field-symbol statement to define a
field-symbol, and use assign to assign a field to it. The
field-symbol name must begin and end with angle brackets.

### Assignment Statements

* Clear
* Move (equivalent to the assignment operator =)
* Move-corresponding

### String Special Comparison Operators

| Operator | Definition               | Case Sensitive | Trailing Blanks Ignored |
| -------- | ------------------------ | -------------- | ----------------------- |
| CO       | Contains Only            | Yes            | No                      |
| CN       | Does not contain only    | Yes            | No                      |
| CA       | Contains Any             | Yes            | No                      |
| NA       | Does not contain any     | Yes            | No                      |
| CS       | Contains String          | No             | Yes                     |
| NS       | Does not contain string  | No             | Yes                     |
| CP       | Contains Pattern         | No             | Yes                     |
| NP       | Does not contain pattern | No             | Yes                     |

*Tip: Use # to escape a character in operators which are not case sensitive*

### Loops

* Do
* While

| Statement | Effect                                       |
| --------- | -------------------------------------------- |
| Exit      | Leaves the current loop                      |
| Continue  | Unconditional jump to the end of the loop    |
| Check exp | Jumps to the end of the loop if exp is false |

## T-Codes

**Table TSTC:** All T-Codes can be viewed

[T-Codes By Category][1]

### Standard T-Codes

*To write an executable report use SE38*

| T-Code         | Transaction       | Description                                                     |
| -------------- | ----------------- | --------------------------------------------------------------- |
| **SE38**       | ABAP Editor       | Write/edit reports, module pools, includes and subroutine pools |
| **SE11, SE12** | View/Change Table | Process database table definitions and retrieve global types    |
| **SE16**       | Table Browser     | View tables in Data Dictionary                                  |
| **SE16N**      | Change Table Data | Interface to change table data in Data Dictionary               |
| **SD11**       | SAP Data Model    |                                                                 |
| **SE41**       | Menu Painter      | Design user interface                                           |
| **SE51**       | Screen Painter    | Design Screen and flow logic                                    |
| **SE37**       | Function Builder  | Write/edit function modules                                     |
| **SE24**       | Class Builder     | Write/edit object classes and interfaces                        |
| **SE80**       | Object Navigator  | Single Interface for various development tools                  |
| **SA38**       | Program Execution | Execute an ABAP program                                         |
| **/NEX**       | Exit              | Exit from the current client                                    |

### Special T-Codes

| T-Code    | Transaction                              | Description                    |
| --------- | ---------------------------------------- | ------------------------------ |
| **ME23N** | Display Purchase Order                   |                                |
| **ME2N**  | Purchasing Documents Per Document Number | Search for purchasing document |
| **MIRO**  | Enter Incoming Invoice                   | Post an Invoice                |

## My Local Development Objects

| Object     | Client  | Type   | Description   |
| ---------- | ------- | ------ | ------------- |
| ZTEST_JC   | DR4 410 | Report | CRUD Practice |
| ztest_jc01 | DR4 410 | Table  | People Table  |

## ABAP CRUD

[Open SQL][8]  

| Operation |           | Code                                                                         |
| --------- | --------- | ---------------------------------------------------------------------------- |
| Create    | POST      | Insert *target* from *workarea* </br> Insert into *target* values *workarea* |
| Read      | GET       | Select Single \* from *table* into *workarea*                                |
| Update    | PUT/PATCH | Update *table* set *workarea*                                                |
| Delete    | DELETE    | Delete from *table*                                                          |
| Query     | GET       | Select \*  from *table* into *table*                                         |

## Internal Tables

[Internal Tables][9]  

### The Append Statement

*append [wa to] [initial line to] it.*  

* *wa* is the name of a work area.  
* *it* is the name of a previously defined internal table.  

*Occurs* does not limit the number of rows that can be added to an internal table. The number of rows you can put into an internal table is theoretically only limited by the amount of virtual memory available on the application server. *Occurs* only serves as a guideline for the program.  

### Reading from Internal Table

1. Loop at
2. Read at

```
loop at it [into wa] [from m] [to n] [where exp].
---
endloop.
```

For reading a single table, use *Read Table*  

```
read table it [into wa] [index i | with key keyexp [binary search] ] [comparing
cmpexp] [transporting texp].
```

## ABAP Code Snippets

### Hello World
<details open>
<summary>Code</summary>

```
REPORT TEST.
WRITE 'Hello World'.
```
</details>

### Get Last Day of Previous Month
<details open>
<summary>Code</summary>

```
DATA LAST_EOM    TYPE D.  "last end-of-month date

* Start from today's date
  LAST_EOM = SY-DATUM.
* Set characters 6 and 7 (0-relative) of the YYYYMMDD string to "01",
* giving the first day of the current month
  LAST_EOM+6(2) = '01'.
* Subtract one day
  LAST_EOM = LAST_EOM - 1.

  WRITE: 'Last day of previous month was', LAST_EOM.
```
</details>

### Explicitly typed declaration
<details open>
<summary>Code</summary>

```
* Primitive types:
DATA: COUNTER      TYPE I,
      VALIDITY     TYPE I VALUE 60,
      TAXRATE(3)   TYPE P DECIMALS 1,
      LASTNAME(20) TYPE C,
      DESCRIPTION  TYPE STRING.

* Dictionary types:
DATA: ORIGIN       TYPE COUNTRY.

* Internal table:
DATA: T_FLIGHTS    TYPE TABLE OF FLIGHTINFO,
      T_LOOKUP     TYPE HASHED TABLE OF FLT_LOOKUP.

* Objects:
DATA: BOOKING      TYPE REF TO CL_FLT_BOOKING.
```
</details>

### Implicit inline declaration (Since ABAP 7.40)
<details open>
<summary>Code</summary>

```
* It must be possible to infer the type statically
DATA(variable_name) = 'VALUE'.

SELECT * FROM ekko into @DATA(lt_ekko) WHERE ebeln EQ @lv_ebeln.
```
</details>

### Define Internal tables
<details open>
<summary>Code</summary>

```
* First define structured type
TYPES: BEGIN OF t_vbrk,
         VBELN TYPE VBRK-VBELN,
         ZUONR TYPE VBRK-ZUONR,
       END OF t_vbrk.

* Now define internal table of our defined type t_vbrk
DATA : gt_vbrk TYPE STANDARD TABLE OF t_vbrk,
       gt_vbrk_2 TYPE STANDARD TABLE OF t_vbrk.   "easy to define more tables

* If needed, define structure (line of internal table)
* Definition with type or with reference to internal table:
DATA : gs_vbrk TYPE t_vbrk,
       gs_vbrk2 LIKE LINE OF gt_vbrk2.

* You can also define table type if needed
TYPES tt_vbrk TYPE STANDARD TABLE OF t_vbrk.
```
</details>

<details open>
<summary>Code</summary>

report ztx1106.
data: begin of it occurs 3,
    f1(2) type n,
    f2 type i,
    f3(2) type c,
    f4 type p,
  end of it,
  wa like it.

it-f1 = '10'. it-f3 = 'AA'. it-f2 = it-f4 = 1. append it.
it-f1 = '20'. it-f3 = 'BB'. it-f2 = it-f4 = 2. append it.
it-f1 = '30'. it-f3 = 'CC'. it-f2 = it-f4 = 3. append it.

write: / 'sy-subrc =', sy-subrc,
  / 'sy-tabix =', sy-tabix,
  / it-f1, it-f2, it-f3, it-f4.

read table it into wa index 1.
write: /,
  / 'sy-subrc =', sy-subrc,
  / 'sy-tabix =', sy-tabix,
  / it-f1, it-f2, it-f3, it-f4,
  / wa-f1, wa-f2, wa-f3, wa-f4.

read table it index 4.
write: /,
  / 'sy-subrc =', sy-subrc,
  / 'sy-tabix =', sy-tabix,
  / it-f1, it-f2, it-f3, it-f4,
  / wa-f1, wa-f2, wa-f3, wa-f4.
</details>

### Field Symbols
<details open>
<summary>Code</summary>

```
report ztx0915.
data f1(3) value 'ABC'.
field-symbols <f>.
assign f1 to <f>. "<f> can now be used in place of f1
write <f>. "writes the contents of f1
<f> = 'XYZ'. "assigns a new value to f1
write / f1.
```
</details>

### Create Internal Table Using Values Construct
<details open>
<summary>Code</summary>

```
TYPES:
  itab1 TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line,
  BEGIN OF struct,
    col1 TYPE c LENGTH 2,
    col2 TYPE c LENGTH 2,
    col3 TYPE c LENGTH 2,
  END OF struct,
  itab2 TYPE SORTED TABLE OF struct WITH UNIQUE KEY col1 col2 col3.

DATA(base1) = VALUE itab1(
                ( `x1y1z1` )
                ( `x2y2z2` )
                ( `x3y3z3` ) ).
DATA(base2) = VALUE itab2(
                ( col1 = 'x1' col2 = 'y1' col3 = 'z1' )
                ( col1 = 'x2' col2 = 'y2' col3 = 'z2' )
                ( col1 = 'x3' col2 = 'y3' col3 = 'z3' ) ).

DATA(tab1) = VALUE #( BASE base1
               ( `A1B1B1` )
               ( `A2B2B2` ) ).

DATA(tab2)  = VALUE #(
                BASE base2
                ( col1 = 'A1' col2 = 'B1' col3 = 'C1' )
                ( col1 = 'A2' col2 = 'B2' col3 = 'C2' ) ).

DATA(tab3) = VALUE itab2( BASE base1
               ( col1 = 'A1' col2 = 'B1' col3 = 'C1' )
               ( col1 = 'A2' col2 = 'B2' col3 = 'C2' ) ).

cl_demo_output=>write(   tab1  ).
cl_demo_output=>write(   tab2 ).
cl_demo_output=>display( tab3 ).
```
</details>

### Case Statement
<details open>
<summary>Code</summary>

```
case v1.
  when v2 [ or vn ... ].
    ---
  when v3 [ or vn ... ].
    ---
  [ when others.
    --- ]
  endcase.
```
</details>

### Loops

<details open>
<summary>Code</summary>

```
do [ v1 times ] [ varying f1 from s-c1 next s-c2 [ varying f2 from s2-c1 next s2-
c2 ... ] ].
---
[exit.]
---
enddo.

while exp [ vary f1 from s-c1 next s-c2 [ vary f2 from s2-c1 next s2-c2 ... ]
  ---
  [ exit. ]
  ---
  endwhile.
```
</details>

## Cool Things

[ABAP in 21 Days HTML][11]  
[ABAP in 21 Days PDF][12]  
[ABAP Wikipedia][2]  
[GURU 99 Tutorial][6]  
[New String Options][3]  
[Value (Quick insert into internal tables)][7]

## TODO List

* [x] Create a table in the DDIC
* [x] Build a CRUD Report
* [ ] Create Search Helps

## VS Code Markdown Help

### Extensions

[Markdown All In One][MD-All-In-One]  
[Run On Save][Run-On-Save]  

### Tips and Shortcuts

Command Palette (Ctrl+Shift+P) -> Print current document in HTML  
Alt+C - Toggle Checklist  
Ctrl+Shift+V - Toggle Markdown  

Add the following to  your markdown snippets and make sure quick suggestions are enabled in settings.json
```json
"Collapsable details block": {
  "prefix": "details",
  "body": [
    "<details open>",
    "<summary>${1:Expand}</summary>",
    "",
    "$2",
    "</details>",
    "$0"
  ]
}
```

[1]: https://www.system-overload.org/sap/transaction-codes.html
[2]: https://blogs.sap.com/2013/05/17/using-new-abap-stuff-new-options-for-strings/
[3]: https://en.wikipedia.org/wiki/ABAP
[4]: https://en.wikipedia.org/wiki/ABAP#Chained_statements
[5]: https://en.wikipedia.org/wiki/ABAP#Spaces
[6]: https://www.guru99.com/abap-tutorial.html
[7]: https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenvalue_constructor_params_itab.htm
[8]: https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenopensql.htm
[9]: https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenitab.htm
[10]: https://www.guru99.com/alv-list-view-programming.html
[11]: ABAP%20in%2021%20Days.html
[12]: abapin21days.pdf
[ABAP-Docu]: http://sapdr4a21.wal-mart.com:8050/sap/public/bc/abap/docu?sap-language=EN&format=STANDARD&object=ABENABAP&tree=X
[MD-All-In-One]: https://marketplace.visualstudio.com/items?itemName=yzhang.markdown-all-in-one
[Run-On-Save]: https://marketplace.visualstudio.com/items?itemName=pucelle.run-on-save
