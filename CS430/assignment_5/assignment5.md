---
title: "CS 430: Assignment 5"
author: Brandon Ingli
# date: \today
# thanks: 
# toc: true
# toc-depth: 4
# indent: true
classoption:
- 11pt
- landscape
geometry:
- margin=1.0in
papersize: letter
header-includes:
- |
 \usepackage{titling}
    \usepackage{fancyhdr}
    \fancypagestyle{document}
        {
            \fancyhead[L]{\theauthor}
            \fancyhead[C]{\thetitle}
            \fancyhead[R]{\thepage}
            \fancyfoot[C]{}
        }
- \fancypagestyle{plain}{\renewcommand{\headrulewidth}{0pt}\fancyhf{}}
- \pagestyle{document}
# - \usepackage{indentfirst}
- \usepackage[margins=raggedright]{floatrow}
- \usepackage{tikz}
- \usetikzlibrary{shapes}
# - \usepackage[parfill]{parskip}
# - \usepackage{mathrsfs}
# - \usepackage{adjustbox}
colorlinks: urlcolor
output: pdf\_document
---

# Question 1

## ER Diagram
![](../assignment3/question1.png)\



## Tables
NOTE: Any table with a ... indicates the table continues on the next line. This is so it fits on the page properly.

### 1NF
| \underline{SSN} | Student_no | Fname | Minit | Lname | Sex | Bday | Class | Degree | ... |
| --------------- | ---------- | ----- | ----- | ----- | --- | ---- | ----- | ------ | --- |

| ... | Local\_address | Local\_city | Local\_state | Local\_zip | Local\_phone | ... |
| --- | -------------- | ----------- | ------------ | ---------- | ------------ | --- |

| ... | Perm\_address | Perm\_city | Perm\_state | Perm\_zip | Perm\_phone | ... |
| --- | ------------- | ---------- | ----------- | --------- | ----------- | --- |

| ... | Major | \underline{Minor} | ... |
| --- | ----- | ----------------- | --- |

| ... | Major_dept_code | Major_dept_name | Major_dept_office_no | Major_dept_phone | Major_dept_college | ... |
| --- | --------------- | --------------- | -------------------- | ---------------- | ------------------ | --- |

| ... | Minor_dept_code | Minor_dept_name | Minor_dept_office_no | Minor_dept_phone | Minor_dept_college | ... |
| --- | --------------- | --------------- | -------------------- | ---------------- | ------------------ | --- |

| ... | Course_dept_code | Course_dept_name | Course_dept_office_no | Course_dept_phone | Course_dept_college | ... |
| --- | ---------------- | ---------------- | --------------------- | ----------------- | ------------------- | --- |

| ... | Course\_name | \underline{Course\_no} | Course\_desc | Course\_level | Course\_hours | ... |
| --- | ------------ | ---------------------- | ------------ | ------------- | ------------- | --- |

| ... | \underline{Section\_no} | \underline{Section\_sem} | \underline{Section\_yr} | \underline{Section\_inst} | ... |
| --- | ----------------------- | ------------------------ | ----------------------- | ------------------------- | --- |

| ... | Grade\_ltr | Grade\_no |
| --- | ---------- | --------- |

### 2NF

#### Student_and_Major_Department
| \underline{SSN} | Student_no | Fname | Minit | Lname | Sex | Bday | Class | Degree | ... |
| --------------- | ---------- | ----- | ----- | ----- | --- | ---- | ----- | ------ | --- |

| ... | Local_address | Local_city | Local_state | Local_zip | Local_phone | ... |
| --- | ------------- | ---------- | ----------- | --------- | ----------- | --- |

| ... | Perm_address | Perm_city | Perm_state | Perm_zip | Perm_phone | Major | ... |
| --- | ------------ | --------- | ---------- | -------- | ---------- | ----- | --- |

| ... | Major_dept_code | Major_dept_name | Major_dept_office_no | Major_dept_phone | Major_dept_college |
| --- | --------------- | --------------- | -------------------- | ---------------- | ------------------ |

#### Minor_Department

| \underline{Minor} | Minor_dept_code | Minor_dept_name | ... |
| ----------------- | --------------- | --------------- | --- |

| ... | Minor_dept_office_no | Minor_dept_phone | Minor_dept_college |
| --- | -------------------- | ---------------- | ------------------ |

#### Student_Minors
| \underline{SSN} | \underline{Minor} |
| --------------- | ----------------- |

#### Course
| \underline{Course\_no} | Course_name | Course_desc | Course_level | Course_hours | ... |
| ---------------------- | ----------- | ----------- | ------------ | ------------ | --- |

| ... | Course_dept_code | Course_dept_name | Course_dept_office_no | Course_dept_phone | Course_dept_college |
| --- | ---------------- | ---------------- | --------------------- | ----------------- | ------------------- |

#### Section
 | \underline{Course\_no} | \underline{Section\_no} | \underline{Section\_sem} | \underline{Section\_yr} | \underline{Section\_inst} |
 | ---------------------- | ----------------------- | ------------------------ | ----------------------- | ------------------------- |

#### Grade
| \underline{SSN} | \underline{Course\_no} | \underline{Section\_no} | \underline{Section\_sem} | \underline{Section\_yr} | \underline{Section\_inst} | Grade_ltr | Grade_no |
| --------------- | ---------------------- | ----------------------- | ------------------------ | ----------------------- | ------------------------- | --------- | -------- |

### 3NF

#### Student
| \underline{SSN} | Student_no | Fname | Minit | Lname | Sex | Bday | Class | Degree | ... |
| --------------- | ---------- | ----- | ----- | ----- | --- | ---- | ----- | ------ | --- |

| ... | Local_address | Local_city | Local_state | Local_zip | Local_phone | ... |
| --- | ------------- | ---------- | ----------- | --------- | ----------- | --- |

| ... | Perm_address | Perm_city | Perm_state | Perm_zip | Perm_phone | Major |
| --- | ------------ | --------- | ---------- | -------- | ---------- | ----- |

#### Majors
| \underline{Major} | Dept_code |
| ----------------- | --------- |

#### Minors
| \underline{Minor} | Dept_code |
| ----------------- | --------- |

#### Departments
| \underline{Dept\_code} | Dept_name | Dept_office_no | Dept_phone | Dept_college |
| ---------------------- | --------- | -------------- | ---------- | ------------ |

#### Student_Minors
| \underline{SSN} | \underline{Minor} |
| --------------- | ----------------- |

#### Course
| \underline{Course\_no} | Course_name | Course_desc | Course_level | Course_hours | Dept_code |
| ---------------------- | ----------- | ----------- | ------------ | ------------ | --------- |

#### Section
 | \underline{Course\_no} | \underline{Section\_no} | \underline{Section\_sem} | \underline{Section\_yr} | \underline{Section\_inst} |
 | ---------------------- | ----------------------- | ------------------------ | ----------------------- | ------------------------- |

#### Grade
| \underline{SSN} | \underline{Course\_no} | \underline{Section\_no} | \underline{Section\_sem} | \underline{Section\_yr} | \underline{Section\_inst} | Grade_ltr |
| --------------- | ---------------------- | ----------------------- | ------------------------ | ----------------------- | ------------------------- | --------- |

#### Grade_Values
| \underline{Grade\_ltr} | Grade_no |
| ---------------------- | -------- |


## FDs and Closures

### FDs
+ SSN $\rightarrow$ Major
+ SSN $\rightarrow$ (Student Demographic Information)
+ SSN $\rightarrow$ Student_no
+ Major $\rightarrow$ {Dept_code, Dept_name}
+ Minor $\rightarrow$ {Dept_code, Dept_name}
+ Dept_name $\rightarrow$ Dept_code
+ Dept_code $\rightarrow$ {Dept_name, Dept_office_no, Dept_phone, Dept_college}
+ Course_no $\rightarrow$ {Course_name, Course_desc, Course_level, Course_hours, Course_dept_code}
+ {Course_no, Section_\*, SSN} $\rightarrow$ Grade_ltr
+ Grade_ltr $\rightarrow$ Grade_no

### Closures
+ SSN $\rightarrow$ Major_dept_\*
+ Student_no $\rightarrow$ Major
+ Student_no $\rightarrow$ (Student Demographic Information)
+ Dept_name $\rightarrow$ {Dept_code, Dept_office_no, Dept_phone, Dept_college}
+ Major $\rightarrow$ {Dept_code, Dept_name, Dept_office_no, Dept_phone, Dept_college}
+ Minor $\rightarrow$ {Dept_code, Dept_name, Dept_office_no, Dept_phone, Dept_college}
+ Course_no $\rightarrow$ {Dept_code, Dept_name, Dept_office_no, Dept_phone, Dept_college}
+ {Course_no, Section_\*, SSN} $\rightarrow$ Grade_no
+ {Course_no, Section_\*, Student_no} $\rightarrow$ Grade_ltr
+ {Course_no, Section_\*, Student_no} $\rightarrow$ Grade_no

# Question 2

## ER Diagram
![](../assignment3/question2.png)\


## Tables

### 1NF
I will assume that, for clarity, any free-agent player or coach will be listed as playing for the team "Free Agent" instead of having a NULL value.

| \underline{Player\_SSN} | Player_name | \underline{Player\_pos} | Player_no | Player_DOB | Player_team_name | Player_team_city | ... |
| ----------------------- | ----------- | ----------------------- | --------- | ---------- | ---------------- | ---------------- | --- |

| ... | \underline{Coach\_SSN} | Coach_name | \underline{Coach\_pos} | Coach_no | Coach_DOB | Coach_team_name | Coach_team_city | ... |
| --- | ---------------------- | ---------- | ---------------------- | -------- | --------- | --------------- | --------------- | --- |

| ... | \underline{Injury\_ID} | Injury_date_on | Injury_date_off | Injury_desc | Injury_treatment | ... |
| --- | ---------------------- | -------------- | --------------- | ----------- | ---------------- | --- |

| ... | \underline{Game\_ID} | Home_team_name | Home_team_city | Away_team_name | Away_team_city | ... |
| --- | -------------------- | -------------- | -------------- | -------------- | -------------- | --- |

| ... | Game_timestamp | Game_innings | Home_score | Away_score |
| --- | -------------- | ------------ | ---------- | ---------- |

### 2NF

#### Player
| \underline{Player\_SSN} | Player_name | Player_DOB | Player_no | Player_team_name | Player_team_city |
| ----------------------- | ----------- | ---------- | --------- | ---------------- | ---------------- |

#### Player_pos
| \underline{Player\_SSN} | \underline{Player\_pos} |
| ----------------------- | ----------------------- |

#### Coach
| \underline{Coach\_SSN} | Coach_name | Coach_DOB | Coach_no | Coach_team_name | Coach_team_city |
| ---------------------- | ---------- | --------- | -------- | --------------- | --------------- |

#### Coach_pos
| \underline{Coach\_SSN} | \underline{Coach\_pos} |
| ---------------------- | ---------------------- |

#### Injury
| \underline{Injury\_ID} | Injured_player_SSN | Date_on | Date_off | Desc | Treatment | ... |
| ---------------------- | ------------------ | ------- | -------- | ---- | --------- | --- |

| ... | Injured_player_name | Injured_player_DOB | Injured_player_no | Injured_player_team_name |
| --- | ------------------- | ------------------ | ----------------- | ------------------------ |

#### Game
| \underline{Game\_ID} | Game_timestamp | Game_innings | Home_team_name | Home_team_score | ... |
| -------------------- | -------------- | ------------ | -------------- | --------------- | --- |

| ... | Away_team_name | Away_team_score | Home_team_city | Away_team_city |
| --- | -------------- | --------------- | -------------- | -------------- |

#### Player_in_game
| \underline{Player\_SSN} | \underline{Game\_ID} | \underline{Player\_pos} |
| ----------------------- | -------------------- | ----------------------- |

#### Coach_in_game
| \underline{Coach\_SSN} | \underline{Game\_ID} | \underline{Coach\_pos} |
| ---------------------- | -------------------- | ---------------------- |

### 3NF

#### Team
| \underline{Team\_name} | Team_city |
| ---------------------- | --------- |

#### Player
| \underline{Player\_SSN} | Player_name | Player_DOB | Player_no | Player_team_name |
| ----------------------- | ----------- | ---------- | --------- | ---------------- |

#### Player_pos
| \underline{Player\_SSN} | \underline{Player\_pos} |
| ----------------------- | ----------------------- |

#### Coach
| \underline{Coach\_SSN} | Coach_name | Coach_DOB | Coach_no | Coach_team_name |
| ---------------------- | ---------- | --------- | -------- | --------------- |

#### Coach_pos
| \underline{Coach\_SSN} | \underline{Coach\_pos} |
| ---------------------- | ---------------------- |

#### Injury
| \underline{Injury\_ID} | Injured_player_SSN | Date_on | Date_off | Desc | Treatment |
| ---------------------- | ------------------ | ------- | -------- | ---- | --------- |

#### Game
| \underline{Game\_ID} | Game_timestamp | Game_innings | Home_team_name | Home_team_score | ... |
| -------------------- | -------------- | ------------ | -------------- | --------------- | --- |

| ... | Away_team_name | Away_team_score |
| --- | -------------- | --------------- |

#### Player_in_game
| \underline{Player\_SSN} | \underline{Game\_ID} | \underline{Player\_pos} |
| ----------------------- | -------------------- | ----------------------- |

#### Coach_in_game
| \underline{Coach\_SSN} | \underline{Game\_ID} | \underline{Coach\_pos} |
| ---------------------- | -------------------- | ---------------------- |

## FDs and Closures

### FDs
+ Player_SSN $\rightarrow$ {Player_name, Player_DOB, Player_no, Player_team_name}
+ Coach_SSN $\rightarrow$ {Coach_name, Coach_DOB, Coach_no, Coach_team_name}
+ Team_name $\rightarrow$ Team_city
+ Injury_ID $\rightarrow$ {Date_on, Date_off, Desc, Treatment, Player_SSN}
+ Game_ID $\rightarrow$ {Game_timestamp, Game_innings, Home_team_name, Home_team_score, Away_team_name, Away_team_score}

### Closures
+ Player_SSN $\rightarrow$ Player_team_city
+ Coach_SSN $\rightarrow$ Coach_team_city
+ Injury_ID $\rightarrow$ {Player_name, Player_DOB, Player_no, Player_team_name}
+ Game_ID $\rightarrow$ {Home_team_city, Away_team_city}

# Question 3
$bfr = \lfloor (B/R) \rfloor = \lfloor (1024\text{ bytes}/400\text{ bytes}) \rfloor = \lfloor 2.56 \rfloor = 2$

$b = \lceil (r/bfr) \rceil = \lceil (3,000,000/2) \rceil = 1,500,000$

## Part a
A linear search of the data would require on average $1,500,000/2 = 750,000$ block accesses.

## Part b
A binary search of the data would require approximately $\lceil \lg b \rceil = \lceil \lg 1,500,000 \rceil = \lceil 20.5165 \rceil = 21$ block accesses.

## Part c

$r_i = 3,000,000$ since this is a dense index.

$R_i = (9 + 8) = 17 \text{ bytes}$

$bfr_i = \lfloor (B/R_i) \rfloor = \lfloor (1024 \text{ bytes} / 17\text{ bytes}) \rfloor = \lfloor 60.2353 \rfloor = 60$

$b_i = \lceil (r_i/bfr_i) \rceil = \lceil (3,000,000 / 60) \rceil = \lceil 50,000 \rceil = 50,000$

A binary search of the index file would require approximately $\lceil \lg b_i \rceil = \lceil \lg 50,000 \rceil = \lceil 15.6096 \rceil = 16$ block accesses, so including the block access to actually retrieve the record, 17 block accesses would be required.

## Part d
Using indexes is almost always better because, in most cases, it reduces the number of sectors you need to search through to find the requested record, due to the usually smaller size and sometimes smaller number of indexes. This results in faster record retrieval speeds due to the smaller amount of mechanical drive motion required.

# Question 4

## Part a
B trees can store data in any node, while B$^+$ trees can only store data in leaf nodes. Additionally, the leaf nodes are linked together in a B$^+$ tree, while they are not linked in a B tree.

## Part b: Insertion and Deletion
I will assume a B$^+$ tree with $p = 3$ and $p_\text{leaf} = 3$

### Insert 7
\begin{tikzpicture}
\tikzstyle{bplus}=[rectangle split,
                    rectangle split horizontal, 
                    rectangle split parts = 3, 
                    rectangle split empty part width=0.01mm, 
                    rectangle split every empty part={\hspace{0.25cm}}, 
                    draw]
\tikzstyle{every node}=[bplus]

\node {\nodepart{one} 7 \nodepart{two}  \nodepart{three} } [->]
;
\end{tikzpicture}

### Insert 3
\begin{tikzpicture}
\tikzstyle{bplus}=[rectangle split,
                    rectangle split horizontal, 
                    rectangle split parts = 3, 
                    rectangle split empty part width=0.01mm, 
                    rectangle split every empty part={\hspace{0.25cm}}, 
                    draw]
\tikzstyle{every node}=[bplus]

\node {\nodepart{one} 3 \nodepart{two} 7 \nodepart{three} } [->]
;
\end{tikzpicture}

### Insert 12
\begin{tikzpicture}
\tikzstyle{bplus}=[rectangle split,
                    rectangle split horizontal, 
                    rectangle split parts = 3, 
                    rectangle split empty part width=0.01mm, 
                    rectangle split every empty part={\hspace{0.25cm}}, 
                    draw]
\tikzstyle{every node}=[bplus]


\node {\nodepart{one} 3 \nodepart{two} 7 \nodepart{three} 12} [->]
;
\end{tikzpicture}

### Insert 9
\begin{tikzpicture}
\tikzstyle{bplus}=[rectangle split,
                    rectangle split horizontal, 
                    rectangle split parts = 3, 
                    rectangle split empty part width=0.01mm, 
                    rectangle split every empty part={\hspace{0.25cm}}, 
                    draw]
\tikzstyle{every node}=[bplus]

\tikzstyle{level 1}=[sibling distance=25mm]

\node {\nodepart{one} 9 \nodepart{two}  \nodepart{three} } [->]
    child 
    {
        node (A) {\nodepart{one} 3 \nodepart{two} 7 \nodepart{three} }
    }
    child 
    {
        node (B) {\nodepart{one} 9 \nodepart{two} 12 \nodepart{three}}
    }
;

\draw [->] (A) -- (B);
\end{tikzpicture}

### Insert 8
\begin{tikzpicture}
\tikzstyle{bplus}=[rectangle split,
                    rectangle split horizontal, 
                    rectangle split parts = 3, 
                    rectangle split empty part width=0.01mm, 
                    rectangle split every empty part={\hspace{0.25cm}}, 
                    draw]
\tikzstyle{every node}=[bplus]

\tikzstyle{level 1}=[sibling distance=25mm]

\node {\nodepart{one} 9 \nodepart{two}  \nodepart{three} } [->]
    child 
    {
        node (A) {\nodepart{one} 3 \nodepart{two} 7 \nodepart{three} 8}
    }
    child 
    {
        node (B) {\nodepart{one} 9 \nodepart{two} 12 \nodepart{three}}
    }
;

\draw [->] (A) -- (B);
\end{tikzpicture}

### Delete 8
\begin{tikzpicture}
\tikzstyle{bplus}=[rectangle split,
                    rectangle split horizontal, 
                    rectangle split parts = 3, 
                    rectangle split empty part width=0.01mm, 
                    rectangle split every empty part={\hspace{0.25cm}}, 
                    draw]
\tikzstyle{every node}=[bplus]

\tikzstyle{level 1}=[sibling distance=25mm]

\node {\nodepart{one} 9 \nodepart{two}  \nodepart{three} } [->]
    child 
    {
        node (A) {\nodepart{one} 3 \nodepart{two} 7 \nodepart{three}}
    }
    child 
    {
        node (B) {\nodepart{one} 9 \nodepart{two} 12 \nodepart{three}}
    }
;

\draw [->] (A) -- (B);
\end{tikzpicture}

### Delete 3
\begin{tikzpicture}
\tikzstyle{bplus}=[rectangle split,
                    rectangle split horizontal, 
                    rectangle split parts = 3, 
                    rectangle split empty part width=0.01mm, 
                    rectangle split every empty part={\hspace{0.25cm}}, 
                    draw]
\tikzstyle{every node}=[bplus]

\tikzstyle{level 1}=[sibling distance=25mm]

\node {\nodepart{one} 7 \nodepart{two} 9 \nodepart{three} 12} [->]
;

\end{tikzpicture}