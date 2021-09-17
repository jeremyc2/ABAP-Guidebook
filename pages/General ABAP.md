<!-- cSpell:disable -->
<!-- title: ABAP Development Notes -->
<script src="https://jeremyc2.github.io/CDN/sync-favicon-with-title.js"></script>
<script src="https://jeremyc2.github.io/CDN/markdown-themes.js"></script>

# ABAP Notes

<details open><summary>Table of Contents</summary>

- [ABAP Notes](#abap-notes)
  - [General Notes](#general-notes)
    - [Enhancements](#enhancements)
    - [Data and Parameters Statements](#data-and-parameters-statements)
    - [Field Symbols](#field-symbols)
    - [Assignment Statements](#assignment-statements)
    - [String Special Comparison Operators](#string-special-comparison-operators)
    - [Loops](#loops)
    - [Events](#events)
    - [Subroutines](#subroutines)
      - [Calling a subroutine](#calling-a-subroutine)
      - [Subroutine Parameters](#subroutine-parameters)
    - [Function modules](#function-modules)
      - [Passing Parameters](#passing-parameters)
    - [Selection screens](#selection-screens)
      - [The Initialization Event](#the-initialization-event)
    - [BADI and BAPI](#badi-and-bapi)
  - [T-Codes](#t-codes)
    - [Standard T-Codes](#standard-t-codes)
    - [Special T-Codes](#special-t-codes)
  - [Useful Tables](#useful-tables)
  - [My Local Development Objects](#my-local-development-objects)
  - [ABAP CRUD](#abap-crud)
  - [Internal Tables](#internal-tables)
    - [The Append Statement](#the-append-statement)
    - [Reading from Internal Table](#reading-from-internal-table)
    - [Obtaining Information about an Internal Table](#obtaining-information-about-an-internal-table)
      - [Is table empty](#is-table-empty)
      - [Get number of rows](#get-number-of-rows)
    - [Copy Data from One Internal Table to another](#copy-data-from-one-internal-table-to-another)
    - [Compare the Contents of Two Internal Tables](#compare-the-contents-of-two-internal-tables)
    - [The editor-call Statement](#the-editor-call-statement)
  - [Write Statement](#write-statement)
    - [The write statement](#the-write-statement)
    - [Writing data](#writing-data)
    - [Using Edit Masks](#using-edit-masks)
    - [As Symbol, as Icon, as Line](#as-symbol-as-icon-as-line)
  - [ABAP Code Snippets](#abap-code-snippets)
    - [Hello World](#hello-world)
    - [Lookup and Open Program by User ID](#lookup-and-open-program-by-user-id)
    - [Get Last Day of Previous Month](#get-last-day-of-previous-month)
    - [Parameters declaration](#parameters-declaration)
    - [Explicitly typed declaration](#explicitly-typed-declaration)
    - [Implicit inline declaration (Since ABAP 7.40)](#implicit-inline-declaration-since-abap-740)
    - [Define Internal tables](#define-internal-tables)
    - [Internal Tables Example 1](#internal-tables-example-1)
    - [Field Symbols](#field-symbols-1)
    - [Create Internal Table Using Values Construct](#create-internal-table-using-values-construct)
    - [Case Statement](#case-statement)
    - [Loops](#loops-1)
    - [Subroutine](#subroutine)
  - [Cool Things](#cool-things)
  - [Task List](#task-list)
  - [VS Code Markdown Help](#vs-code-markdown-help)
    - [Extensions](#extensions)
    - [Tips and Shortcuts](#tips-and-shortcuts)
</details>

## General Notes

Quick Tip: You can lookup recently used T-Codes and programs on any given date or by any given user with the T-Code STAD

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

Find Reports by User:
  * Use table TRDIR

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

> Q How do I decide whether to use an include, an internal subroutine, an external subroutine, or a function module to implement my code?  
  
> A If the code will not be used by any other programs, then use an internal subroutine. If the code might be useful to other programs, use a
function module. You should not create external subroutines. They were only covered so that you would know how to
use them because there are still many in use in the R/3 system. It's also easier to understand function modules if you know how external
subroutines work. Instead of external subroutines, use function modules. Includes should be used to simplify your program structure and
group similar components together. They should not be used as containers for reusable code that is included into multiple programs. For
example, you might put all of your data declarations into one include, your events into another, your subroutines into a third, and your
calls to function modules into a fourth.  

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

### Enhancements

[Enhancements PDF Guidebook][13]

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

Using message classes:  
*Don't forget to specify the correct message ID at the beginning of the report*  
*Use system message classes before making your own*  

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

### Events

Each executable program contains a standard selection screen with the screen
number 1000. The screen elements on the standard selection screen are defined by
all PARAMETERS, SELECT-OPTIONS, and SELECTION-SCREEN statements that are not
within the definition of a stand-alone selection screen, in other words, that
are not defined between the following statements:  
  
```
SELECTION-SCREEN BEGIN OF SCREEN ...  
...  
SELECTION-SCREEN END OF SCREEN ...  
```
  
If a standard selection screen comprises the screen for the executable program
and the screen for a logical database, the screen elements for the executable
program are listed below those for the logical database.  

Function groups and module pools do not have a standard selection screen. Here,
you must place the three statements PARAMETERS, SELECT-OPTIONS, and
SELECTION-SCREEN within the definition of the stand-alone selection screen.  

**It is advisable to group all the statements that define the standard selection
screen and list them in the global declaration section together with the
definitions of stand-alone selection screens.**  

| Category | Events                                                                                             |
| -------- | -------------------------------------------------------------------------------------------------- |
| Driver   | initialization </br> at selection-screen </br> start-of-selection </br> get </br> end-of-selection |
| User     | at line-selection </br> at pfn </br> at user-command                                               |
| Program  | top-of-page </br> end-of-page                                                                      |

**LOAD-OF-PROGRAM**   
First event to be called before any of the other ABAP code is processed  
  
**INITIALIZATION**  
Called after the abap selection screen code has been processed (i.e. parameters,  
select-options etc) but before these are displayed to the user. So you can use  
it to initialize input fields of the selection screen or change the default  
values of these before the user gets to enter data into them.  
  
**AT SELECTION SCREEN OUTPUT**  
This is called just before the selection screen is displayed and can be used to  
manipulate the actual selection screen attributes using the loop at screen  
functionality. This allows you to do things like hide fields, grey them out so  
they are output only or make them intensified etc.  
  
```
LOOP AT SCREEN.  
  IF SCREEN-name = 'P_FIELd1'.  
    SCREEN-INTENSIFIED = '1'. MODIFY SCREEN. CONTINUE.  
  ENDIF.  
ENDLOOP.  
```
  
**AT SELECTION-SCREEN**  
This event is triggered when the user presses enter on the report selection  
screen and is mainly used to add processing related to screen validation. Within  
the AT SELECTION SCREEN event you have not left the selection screen, which  
allows you to manipulate the values contained within sel screen parameters or  
stop the program and return the user to the selection screen by displaying an  
error message.  
  
**AT SELECTION-SCREEN ON**  
This allows you to add processing to a selection screen field for when it is  
changed, user presses F1 or F4 etc using the following parameters  
  
```
<parameter1>  
AT SELECTION-SCREEN ON Block <block1>  
AT SELECTION-SCREEN ON HELP-REQUEST for <parameter1> (i.e. When press F1)  
AT SELECTION-SCREEN ON VALUE-REQUEST for <parameter1> (i.e. When press F4)  
AT SELECTION-SCREEN ON RADIOBUTTON for <parameter1> (i.e. When press F4)  
  
AT SELECTION-SCREEN ON P_FIELD1. AT SELECTION-SCREEN ON help-request for  
P_FIELD1.  
```
  
**TOP-OF-PAGE**  
This is called when a new page is started with an ABAP list and is used to  
display a header for the list.  
  
**TOP-OF-PAGE During LINE_SELECTION**  
If you use the 'DURING LINE-SELECTION' addition this event is also triggered  
when creating detailed lists.  
  
**END-OF-PAGE**  
This is displayed at the end of each page if there is a line reservation in the  
addition LINE-COUNT of the initiating statement for a page footer.  
  
```
REPORT demo_rep NO STANDARD PAGE HEADING  
                            LINE-COUNT 0(1).  
```
  
**START-OF-SELECTION**  
If the user executes the program either by pressing the execute button or F8  
this is the first event triggered after all screen selection processing has been  
completed (AT Selection...). So pressing execute triggers both AT SELECTION and  
then START-OF-SELECTION  
  
If no other events are declared then there is no need to manually code this  
event as all processing statements will be automatically assigned to an implicit  
START-OF-SELECTION block. It is a good ideal to put it in though as it separates  
the code and makes it easier to read. Generally you would put all you data  
selection ABAP code within this event.  
  
**END-OF-SELECTION**  
If your report is linked to a logical database this event is called after the  
logical database has completely finished its work. Otherwise this event is  
triggered after the START-OF-SELECTION event so you would generally use it to  
processes the data retrieved in that event.

| Location                                    | Event      | Action                                                                                          |
| ------------------------------------------- | ---------- | ----------------------------------------------------------------------------------------------- |
| before start-of-selection                   | exit/check | exit event                                                                                      |
|                                             | stop       | go to end-of-selection                                                                          |
| in start-of-selection and subsequent events | exit       | terminates report and shows output list </br> (except in top-of-page where it leaves the event) |
|                                             | check      | exits event                                                                                     |
|                                             | stop       | go to end-of-selection event                                                                    |
| initialization                              | exit/check | exits event                                                                                     |
|                                             | stop       | don't do this                                                                                   |
| at selection-screen output                  | exit/check | exits event                                                                                     |
|                                             | stop       | don't do this                                                                                   |
| at selection-screen on radiobutton group g1 | exit/check | exits event                                                                                     |
|                                             | stop       | goes to end-of-selection                                                                        |
| at selection-screen                         | exit/check | exits event                                                                                     |
|                                             | stop       | goes to end-of-selection                                                                        |
| start-of-selection                          | exit       | exits report                                                                                    |
|                                             | check      | exits event                                                                                     |
|                                             | stop       | goes to end-of-selection                                                                        |
| end-of-selection                            | exit       | exits report                                                                                    |
|                                             | check      | exits report                                                                                    |
|                                             | stop       | exits report                                                                                    |
| top-of-page                                 | exit/check | exits event and returns to write statement                                                      |
|                                             | stop       | go to end-of-selection - do not write after it                                                  |
| end-of-page                                 | exit       | exits report                                                                                    |
|                                             | check      | exits event and returns to write statement                                                      |
|                                             | stop       | go to end-of-selection - don't write after it                                                   |

### Subroutines

form s [tables t1 t2 ...]  
       [using u1 value(u2) ...]  
       [changing c1 value(c2) ...].  
  
endform.  

* tables allows internal tables to be passed as parameters.
* The value addition cannot be used after tables.
* The value addition can be applied to any variables passed via using or changing.
* All additions are optional.
* When they are coded, additions must appear in the order shown here. If coded, tables must come first, then using, and then changing.
* Each addition can only be specified once. For example, the tables addition can only appear once. However, multiple tables can appear after it.
* Do not use commas to separate parameters.
* tables only allows internal tables to be passed-not database tables.
* A subroutine can call another subroutine.
* Recursion is supported. A subroutine can call itself or a subroutine that calls it.
* Subroutine definitions cannot be nested. (You cannot define a subroutine within another subroutine.)

#### Calling a subroutine

perform a) s  
        b) n of s1 s2 s3 ...  
                      [tables t1 t2 ...]  
                      [using u1 u2 ...]  
                      [changing c1 c2 ...].  

s, s1, s2, s3, are subroutine names.
* n is a numeric variable.
* a) and b) are mutually exclusive.
* tables, using, and changing can appear with either a) or b).
* The addition value() cannot be used with perform.

Using syntax b) you can specify that one of a list of subroutines be performed. The nth subroutine in the list of subroutine names is
performed. For example, if n is 2, the second subroutine in the list will be performed.

#### Subroutine Parameters

| Addition           | Method                   |
| ------------------ | ------------------------ |
| using v1           | Pass by reference        |
| hanging v1         | Pass by reference        |
| dusing value(v1)   | Pass by value            |
| changing value(v1) | Pass by value and result |

### Function modules

Similar to Subroutine:  
* Both exist within an external program.  
* Both enable parameters to be passed and returned.  
* Parameters can be passed by value, by value and result, or by reference.  


The major differences between function modules and external subroutines are the following:  
* Function modules have a special screen used for defining parameters-parameters are not defined via ABAP/4 statements.  
* tables work areas are not shared between the function module and the calling program.  
* Different syntax is used to call a function module than to call a subroutine.  
* Leaving a function module is accomplished via the *raise* statement instead of *check*, *exit*, or *stop*.  

#### Passing Parameters

To pass parameters to a function module, you must define a *function module interface*, also called an *interface*.  
  
call function 'F'  
          [exporting p1 = v1 ... ]  
          [importing p2 = v2 ... ]  
          [changing p3 = v3 ... ]  
          [tables p4 = it ... ]  
          [exceptions x1 = n [others = n]].  
  
where:
* F is the function module name.
* p1 through p4 are parameter names defined in the function module interface.
* v1 through v3 are variable or field string names defined within the calling program.
* it is an internal table defined within the calling program.
* n is any integer literal; n cannot be a variable.
* x1 is an exception name raised within the function module.  
  
The following points apply:
* All additions are optional.
* call function is a single statement. Do not place periods or commas after parameters or exception names.
* The function module name must be coded in uppercase. If it is coded in lowercase, the function will not be found and a short dump will result.

### Selection screens

It is good practice to define sequential processing blocks in the order by which they will most likely be triggered during
selection screen execution.  
  
Most important events (in most likely order):  
* The initialization event  
* The at selection-screen event  
* The at user-command event  

#### The Initialization Event

The at selection-screen event is processed after user input on the active selection screen. This can occur when the user presses a function key or
clicks a pushbutton, as well as a host of other elements that can be interacted on by the user. In addition to data validation checks, warning messages,
GUI status change, or even pop-up windows can be called using the at selection-screen event.

### BADI and BAPI

BAPI allows connecting SAP systems with SAP or non-SAP systems  
BADI allows adding enhancements to improve the functionalities without affecting the initial source codes.  
  
## T-Codes

**Table TSTC:** All T-Codes can be viewed

[T-Codes By Category][1]

### Standard T-Codes

*To write an executable report use SE38*

| T-Code         | Transaction         | Description                                                     |
| -------------- | ------------------- | --------------------------------------------------------------- |
| **SE38**       | ABAP Editor         | Write/edit reports, module pools, includes and subroutine pools |
| **SE11, SE12** | View/Change Table   | Process database table definitions and retrieve global types    |
| **SE16**       | Table Browser       | View tables in Data Dictionary                                  |
| **SE18**       | Enhancement Spot    | Create and Edit enhancements (including BADIs)                  |
| **SE16N**      | Change Table Data   | Interface to change table data in Data Dictionary               |
| **SD11**       | SAP Data Model      |                                                                 |
| **SE41**       | Menu Painter        | Design user interface                                           |
| **SE51**       | Screen Painter      | Design Screen and flow logic                                    |
| **SE37**       | Function Builder    | Write/edit function modules                                     |
| **SE24**       | Class Builder       | Write/edit object classes and interfaces                        |
| **SE80**       | Object Navigator    | Single Interface for various development tools                  |
| **SA38**       | Program Execution   | Execute an ABAP program                                         |
| **ST22**       | Dump Analysis       | View latest error dump                                          |
| **SU53**       | Authorization Check | Authorization check for troubleshooting                         |
| **/H**         | Debug Mode          | Start debugger after next user action                           |
| **/NEX**       | Exit                | Exit from the current client                                    |

### Special T-Codes

| T-Code    | Transaction                              | Description                    |
| --------- | ---------------------------------------- | ------------------------------ |
| **ME23N** | Display Purchase Order                   |                                |
| **ME2N**  | Purchasing Documents Per Document Number | Search for purchasing document |
| **MIRO**  | Enter Incoming Invoice                   | Post an Invoice                |

## Useful Tables

| Table                     | Description                |
| ------------------------- | -------------------------- |
| ZRHRDW_ENH612_REMEDIATION | Update user data for Ariba |

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

Use *Sort **it** by text* to group similar characters together (i.e. accented characters near their unaccented counterparts)
instead of sorting by ASCII code.

Use the following constructs to test and modify the contents of internal tables:
   * The table body operator
   * describe table
   * append lines
   * insert lines
   * editor-call
   * insert
   * modify
   * free
   * delete
   * clear
   * refresh
   * append sorted by
   * collect


The body of an internal table is represented by the syntax it[] (nothing between the brackets), where it is the name of any internal table.  
  
You can use this syntax to perform efficient table operations that do not require the use of a header line.  
If an internal table does not have a header line, the internal table name itself represents the body. For example, if internal table it does not have a header line, you can use either it[] or it to represent the body; they are equivalent.  

### Obtaining Information about an Internal Table

#### Is table empty

*if it[] is initial*  

#### Get number of rows

1. *describe table it [lines i] [occurs j].*
2. *sy-tfill*

Other variables:
  * sy-tleng - length of a row in bytes
  * sy-toccu - current value of the *occurs* clause

### Copy Data from One Internal Table to another

*it2[] = it1[].*

append lines of it1 [from nf] [to nt] to it2.

* it1 and it2 are internal tables with or without header lines.
* nf and nt are numeric variables, literals, or constants.

**Using append lines is three to four times faster than using append to add the rows one at a time.**

*insert lines of it1 [from nf] [to nt] into it2 [index nb].*

### Compare the Contents of Two Internal Tables

*if it1[] = it2[].*

### The editor-call Statement

The editor-call statement displays the contents of an internal table to the user in an editor similar to the ABAP/4 source code editor. It is useful for debugging and as a simple interface for allowing the user to enter and modify data.

*editor-call for it [title t] [display mode]*

## Write Statement

### The write statement

write [at] [/p(l)] v1[+o(sl)]  
(1) under v2 | no-gap  
(2) using edit mask m | using no edit mask  
(3) mm/dd/yy | dd/mm/yy  
(4) mm/dd/yyyy | dd/mm/yyyy  
(5) mmddyy | ddmmyy | yymmdd  
(6) no-zero  
(7) no-sign  
(8) decimals n  
(9) round n  
(10) currency c | unit u  
(11) left-justified | centered | right-justified  

where:
* v1 is a literal, variable, or field string name.  
* p, l, and n are numeric literals or variables.  
* p is the position specification. It identifies the output column in which the output field should begin.  
* l is the length specification. It identifies the length of the output field in which the value should be written.  
* o is a subfield offset specification that can contain numeric literals only.  
* sl is a subfield length specification that can contain numeric literals only.  
* m is an edit mask.  
* c is a currency key (from table tcurc).  
* u is a unit of measure (from table t006).  
* n is a numeric literal or variable.  

The following points apply:  
* If either p or l is a variable, the word at is required.  
* A slash (/) begins a new line before writing the variable value.  

### Writing data

| Data | Justification | Format                      | Sign     | Dflt Output Type Length  |
| ---- | ------------- | --------------------------- | -------- | ------------------------ |
| i    | Right         | Zeros suppressed            | Trailing | 11                       |
| p    | Right         | Zeros suppressed            | Trailing | 2*fldlen or (2*fldlen)+1 |
| f    | None          | Scientific notation         | Leading  | 22                       |
| n    | Right         | Leading zeros shown         | None     | fldlen                   |
| c    | Left          | Leading blanks suppressed   |          | fldlen                   |
| d    | Left          | Determined by user defaults |          | 8                        |
| t    | Left          | HH:MM:SS                    |          | 6                        |
| x    | Left          |                             |          | 2*fldlen                 |

### Using Edit Masks

Edit masks enable you to:
* Insert characters into the output  
* Move the sign to the beginning of a numeric field  
* Artificially insert or move a decimal point  
* Display a floating-point number without using scientific notation  

Use underscore for placeholders  

### As Symbol, as Icon, as Line

| Type      | Description                       |
| --------- | --------------------------------- |
| as symbol | Displays a black-and-white symbol |
| as icon   | Displays a color icon             |
| as line   | Displays a line-draw character    |

## ABAP Code Snippets

### Hello World
<details open>
<summary>Code</summary>

```
REPORT TEST.
WRITE 'Hello World'.
```
</details>

### Lookup and Open Program by User ID
<details open>
<summary>Code</summary>

```
TABLES trdir.
SELECT-OPTIONS user FOR trdir-cnam NO INTERVALS.

TYPES: BEGIN OF t_user,
         id   like user_addr-bname,
         name like user_addr-name_textc,
       END OF t_user,
       BEGIN OF t_res,
         user         LIKE user_addr-name_textc,
         program_name LIKE trdir-name,
       END OF t_res.

DATA: wa_trdir TYPE trdir,
      it_res   TYPE TABLE OF t_res,
      it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      layout TYPE SLIS_LAYOUT_ALV.

wa_fieldcat-fieldname  = 'USER'.    " Fieldname in the data table
wa_fieldcat-seltext_m  = 'User'.   " Column description in the output
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'PROGRAM_NAME'.
wa_fieldcat-seltext_m  = 'Program Name'.
APPEND wa_fieldcat TO it_fieldcat.

layout-colwidth_optimize = 'X'.

SELECT * FROM trdir into wa_trdir WHERE cnam IN user AND name NOT LIKE '!%' ORDER BY cnam.
  SELECT SINGLE name_textc FROM user_addr INTO @DATA(full_name) WHERE bname EQ @wa_trdir-cnam.
  APPEND VALUE #( user = full_name program_name = wa_trdir-name ) TO it_res.
endselect.

CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
  EXPORTING
    i_callback_program = sy-repid
    it_fieldcat = it_fieldcat
    is_layout = layout
    i_callback_user_command = 'HANDLE_USER_COMMAND'
  TABLES
    t_outtab    = it_res.

FORM f4_opts.
  DATA it_user TYPE TABLE OF t_user.

  SELECT bname, name_textc FROM user_addr INTO TABLE @it_user.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'ID'
      dynpprog    = 'ZTEST_JC'
      dynpnr      = '1000'
      dynprofield = 'USER'
      value_org   = 'S'
    TABLES
      value_tab   = it_user.
ENDFORM.

FORM handle_user_command USING r_ucomm LIKE sy-ucomm "User commands
rs_selfield TYPE slis_selfield. "Field selected
     CASE r_ucomm.
       WHEN '&IC1'. "Double-click command
         IF rs_selfield-fieldname = 'PROGRAM_NAME'.

             " Not using this method anymore because we want to start in
             " a new task
*            DATA: bdcdata_tab TYPE TABLE OF bdcdata,
*                  opt TYPE ctu_params.
*
*            bdcdata_tab = VALUE #(
*              ( program  = 'SAPLWBABAP' dynpro = '0100' dynbegin = 'X' )
*              ( fnam = 'RS38M-PROGRAMM' fval = rs_selfield-value ) ).

*            TRY.
*                CALL TRANSACTION 'SE38' WITH AUTHORITY-CHECK
*                                        USING bdcdata_tab OPTIONS FROM opt.
*              CATCH cx_sy_authorization_error ##NO_HANDLER.
*            ENDTRY.

              DATA: wa_spagpa TYPE rfc_spagpa,
                    it_spagpa TYPE TABLE OF rfc_spagpa.

              wa_spagpa-parid  = 'RID'.
              wa_spagpa-parval  = rs_selfield-value.
              APPEND wa_spagpa TO it_spagpa.

              DATA t(8).
              t = 'tid001'.
              CALL FUNCTION 'ABAP4_CALL_TRANSACTION'  STARTING NEW TASK t
                EXPORTING
                  TCODE = 'SE38'
                TABLES
                  SPAGPA_TAB = it_spagpa.

         ENDIF.
     ENDCASE.

ENDFORM. "HANDLE_USER_COMMAND

AT SELECTION-SCREEN ON VALUE-REQUEST FOR user-low.
  PERFORM f4_opts.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR user-high.
  PERFORM f4_opts.
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

### Parameters declaration
<details open>
<summary>Code</summary>

```
PARAMETERS <P_NAME> TYPE <TABLE-FIELD>. " General parameter for a input field

PARAMETERS <P_NAME> TYPE <TABLE-FIELD> OBLIGATORY. "Parameter for mandatory input field

PARAMETERS <P_NAME> AS CHECKBOX. " Parameter for check box printing


PARAMETERS <P_NAME1> RADIOBUTTONGROUP <RADIOBUTTON GROUP>. " Print Radio button group
PARAMETERS <P_NAME2> RADIOBUTTONGROUP <RADIOBUTTON GROUP>. " Print Radio button group
PARAMETERS <P_NAME2> RADIOBUTTONGROUP <RADIOBUTTON GROUP>. " Print Radio button group
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

### Internal Tables Example 1
<details open>
<summary>Code</summary>

```
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
```
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

### Subroutine

<details open>
<summary>Code</summary>

```
report ztx1706.

write: / 'Before call 1'.
perform sub1.
write: / 'Before call 2'.
perform sub1.
write: / 'After calls'.

form sub1.
write: / 'Inside sub1'.
endform.
```
</details>

<details open>
<summary>Code</summary>

```
report YJACOBJX message-id Y6.
* Database Table Definitions
tables: mara.
selection-screen skip 1.
    selection-screen begin of block block0 with frame title text-000.
    selection-screen skip 1.
selection-screen begin of line.
selection-screen pushbutton 10(20) text-003 user-command engl.
selection-screen pushbutton 50(20) text-004 user-command germ.
selection-screen end of line.
    selection-screen end of block block0.
* Selection parameters
selection-screen skip 2.
selection-screen begin of block block1 with frame title text-001 no intervals.

selection-screen begin of line.
parameters: p_ex1 radiobutton group rad1 .
selection-screen comment 5(30) text-ex1.
selection-screen end of line.
parameters: p_jdate1 type d default sy-datum.
selection-screen skip 1.
selection-screen begin of line.
parameters: p_ex2 radiobutton group rad1 .
selection-screen comment 5(30) text-ex2.
selection-screen end of line.
select-options: s_jdate2 for mara-laeda.
selection-screen skip 1.
selection-screen begin of line.
parameters: p_ex3 radiobutton group rad1.
selection-screen comment 5(20) text-ex3.
selection-screen end of line.
parameters: p_jdate3 like mara-laeda.
selection-screen skip 1.
selection-screen begin of line.
parameters: p_ex4 radiobutton group rad1 .
selection-screen comment 5(30) text-ex4.
selection-screen end of line.
select-options: s_jdate4 for mara-laeda no-extension no intervals.
selection-screen end of block block1.
selection-screen skip.
selection-screen begin of block block2 with frame title text-002 no intervals.

selection-screen begin of line.
parameters: P_ex5 as checkbox.
selection-screen comment 5(30) text-ex5.
selection-screen end of line.
selection-screen skip.
selection-screen begin of line.
parameters: P_ex6 as checkbox.
selection-screen comment 5(30) text-ex6.
selection-screen end of line.
selection-screen skip.
selection-screen begin of line.
parameters: P_ex7 as checkbox.
selection-screen comment 5(30) text-ex7.
selection-screen end of line.
selection-screen end of block block2.
* AT selection-screen.
AT selection-screen.
    if ( p_ex1 = 'X' ) and
    ( ( p_jdate1 = 'IEQ?' ) or ( p_jdate1 is initial ) ).
        message E017 with 'Selection Option with Default field has no value'.
    elseif ( p_ex1 = 'X' ) and
    not ( ( p_jdate1 = 'IEQ?' ) or ( p_jdate1 is initial ) ).
        message I017 with 'We are now using Example 1'.
    endif.
    if ( p_ex2 = 'X' ) and
    ( ( s_jdate2 = 'IEQ?' ) or ( s_jdate2 is initial ) ).
        message E017 with 'Selection Option using for field has no value'.
    elseif ( p_ex2 = 'X' ) and
    not ( ( s_jdate2 = 'IEQ?' ) or ( s_jdate2 is initial ) ).
        message I017 with 'And now Example 2 is selected'.
    endif.
    if ( p_ex3 = 'X' ) and
    ( ( p_jdate3 = 'IEQ?' ) or ( p_jdate3 is initial ) ).
        message E017 with 'Parameter w/ like statement field has no value'.
    elseif ( p_ex3 = 'X' ) and
    not ( ( p_jdate3 = 'IEQ?' ) or ( p_jdate3 is initial ) ).
        message I017 with 'We are now using Example 3'.
    endif.
    if ( p_ex4 = 'X' ) and
    ( ( s_jdate4 = 'IEQ?' ) or ( s_jdate4 is initial ) ).
        message E017 with 'Selection Option with no interval has no value'.
    elseif ( p_ex4 = 'X' ) and
    not ( ( s_jdate4 = 'IEQ?' ) or ( s_jdate4 is initial ) ).
        message I017 with 'We are now using Example 4'.
    endif.
    if p_ex5 = 'X'.
        perform get_price_data.
    else.
        message I017 with 'No Pricing Data selected'.
    endif.
    if p_ex6 = 'X'.
        perform get_cost_data.
    else.
        message I017 with 'No Costing Data selected'.
    endif.
    if p_ex7 = 'X'.
        perform get_revenue_data.
    else.
        message I017 with 'No Revenue Data selected'.
    endif.
    form get_cost_data.
        ...
    endform.
    form get_revenue_data.
        ...
    endform.
    form get_price_data.
        ...
    endform.
```
</details>

## Cool Things

[ABAP in 21 Days HTML][11]  
[ABAP in 21 Days PDF][12]  
[ABAP Wikipedia][2]  
[GURU 99 Tutorial][6]  
[New String Options][3]  
[Value (Quick insert into internal tables)][7]

## Task List

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
[13]: SAP%20Enhancement%20Framework.pdf
[ABAP-Docu]: http://sapdr4a21.wal-mart.com:8050/sap/public/bc/abap/docu?sap-language=EN&format=STANDARD&object=ABENABAP&tree=X
[MD-All-In-One]: https://marketplace.visualstudio.com/items?itemName=yzhang.markdown-all-in-one
[Run-On-Save]: https://marketplace.visualstudio.com/items?itemName=pucelle.run-on-save
