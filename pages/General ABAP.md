<!-- title: ABAP Development Notes -->
<script src="https://jeremyc2.github.io/CDN/markdown-themes.js"></script>

# SAP Tables

<details open><summary>Table of Contents</summary>

- [SAP Tables](#sap-tables)
  - [General Notes](#general-notes)
  - [T-Codes](#t-codes)
    - [Standard T-Codes](#standard-t-codes)
    - [Special T-Codes](#special-t-codes)
  - [My Local Development Objects](#my-local-development-objects)
  - [ABAP CRUD](#abap-crud)
  - [ABAP Code Snippets](#abap-code-snippets)
    - [Hello World](#hello-world)
    - [Get Last Day of Previous Month](#get-last-day-of-previous-month)
    - [Explicitly typed declaration](#explicitly-typed-declaration)
    - [Implicit inline declaration (Since ABAP 7.40)](#implicit-inline-declaration-since-abap-740)
    - [Define Internal tables](#define-internal-tables)
    - [Create Internal Table Using Values Construct](#create-internal-table-using-values-construct)
  - [Cool Things](#cool-things)
  - [TODO List](#todo-list)
  - [VS Code Markdown Help](#vs-code-markdown-help)
    - [Extensions](#extensions)
    - [Tips and Shortcuts](#tips-and-shortcuts)
</details>

## General Notes

ABAP is interpreted.  
ABAP is [Whitespace sensitive][5].  
ABAP is not case-sensitive.  
Statements end with a period.  
Remember [Chained Statements][4]  

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

Message types:
  * A: Abend
  * E: Error
  * I: Information
  * S: Success, Status
  * W: Warning
  * X: Exit (Terminates and logs info to T-Code ST22)

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

### Special T-Codes

## My Local Development Objects

| Object     | Client  | Type   | Description   |
| ---------- | ------- | ------ | ------------- |
| ZTEST_JC   | DR4 410 | Report | CRUD Practice |
| ztest_jc01 | DR4 410 | Table  | People Table  |

## ABAP CRUD

[Open SQL](https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenopensql.htm)

| Operation |           | Code                                                                         |
| --------- | --------- | ---------------------------------------------------------------------------- |
| Create    | POST      | Insert *target* from *workarea* </br> Insert into *target* values *workarea* |
| Read      | GET       | Select Single \* from *table* into *workarea*                                |
| Update    | PUT/PATCH | Update *table* set *workarea*                                                |
| Delete    | DELETE    | Delete from *table*                                                          |
| Query     | GET       | Select \*  from *table* into *table*                                         |

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

## Cool Things

[ABAP Wikipedia][2]  
[GURU 99 Tutorial][6]  
[New String Options][3]  
[Value (Quick insert into internal tables)][7]

## TODO List

* [x] Create a table in the DDIC
* [ ] Build a CRUD Report
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
[MD-All-In-One]: https://marketplace.visualstudio.com/items?itemName=yzhang.markdown-all-in-one
[Run-On-Save]: https://marketplace.visualstudio.com/items?itemName=pucelle.run-on-save
