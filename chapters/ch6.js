export const content = {
    explanation: `
        <h2>Modular Programming with Subprograms</h2>
        <p>As programs grow, it's essential to break them down into smaller, manageable, and reusable pieces. In COBOL, this is achieved by creating <strong>subprograms</strong> (or subroutines) that can be 'called' by a main program.</p>
        
        <h2>The Main Program: <code>CALL</code></h2>
        <p>The main program uses the <code>CALL</code> statement to execute a subprogram. You can pass data to the subprogram using the <code>USING</code> phrase.</p>
        <pre><code class="language-cobol">
*> MAIN-PROGRAM
...
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-NUM1      PIC 99 VALUE 10.
01 WS-NUM2      PIC 99 VALUE 25.
01 WS-RESULT    PIC 9(3).
...
PROCEDURE DIVISION.
    CALL 'SUBPROG1' USING WS-NUM1, WS-NUM2, WS-RESULT.
    DISPLAY 'Result from subprogram: ' WS-RESULT.
    STOP RUN.
        </code></pre>
        <p>In this example, <code>'SUBPROG1'</code> is the program-id of the subprogram to be executed. We are passing three variables to it.</p>

        <h2>The Subprogram: <code>LINKAGE SECTION</code></h2>
        <p>The subprogram is a complete COBOL program, but with two key differences: the <code>LINKAGE SECTION</code> and the <code>EXIT PROGRAM</code> statement.</p>
        <p>The <strong><code>LINKAGE SECTION</code></strong> is a special section in the <code>DATA DIVISION</code> that defines variables to receive the data passed from the calling program. These variable names don't have to match the main program's, but their order, size, and type must.</p>
        <pre><code class="language-cobol">
*> SUB-PROGRAM
IDENTIFICATION DIVISION.
PROGRAM-ID. SUBPROG1.
DATA DIVISION.
LINKAGE SECTION.
01 LS-VAL1       PIC 99.
01 LS-VAL2       PIC 99.
01 LS-SUM        PIC 9(3).

PROCEDURE DIVISION USING LS-VAL1, LS-VAL2, LS-SUM.
    ADD LS-VAL1, LS-VAL2 GIVING LS-SUM.
    EXIT PROGRAM.
        </code></pre>
        <ul>
            <li>The <code>PROCEDURE DIVISION USING...</code> statement links the passed data to the variables defined in the <code>LINKAGE SECTION</code>.</li>
            <li>When the subprogram modifies a linkage variable (like <code>LS-SUM</code>), it modifies the corresponding variable in the main program (<code>WS-RESULT</code>). This is known as passing 'by reference'.</li>
            <li><strong><code>EXIT PROGRAM</code></strong> is used instead of <code>STOP RUN</code> to return control to the statement immediately after the <code>CALL</code> in the main program.</li>
        </ul>
    `,
    exercise: `
        <h2>Creating a Formatter Subprogram</h2>
        <p><strong>Task:</strong> Create a main program that defines a first name and a last name. Call a subprogram to combine them into a single 'Last Name, First Name' format.</p>
        <p><strong>Main Program Steps:</strong>
            <ol>
                <li>In <code>WORKING-STORAGE</code>, define <code>WS-FNAME PIC X(10)</code>, <code>WS-LNAME PIC X(10)</code>, and <code>WS-FULLNAME PIC X(22)</code>.</li>
                <li>Move 'John' to <code>WS-FNAME</code> and 'Smith' to <code>WS-LNAME</code>.</li>
                <li><code>CALL 'FORMATNM' USING WS-FNAME, WS-LNAME, WS-FULLNAME</code>.</li>
                <li><code>DISPLAY</code> the resulting <code>WS-FULLNAME</code>.</li>
            </ol>
        </p>
         <p><strong>Subprogram ('FORMATNM') Steps:</strong>
            <ol>
                <li>In the <code>LINKAGE SECTION</code>, define corresponding fields: <code>LS-FNAME</code>, <code>LS-LNAME</code>, <code>LS-FULLNAME</code>.</li>
                <li>The <code>PROCEDURE DIVISION</code> must have the <code>USING</code> phrase.</li>
                <li>Use the <code>STRING</code> verb to combine the names: <code>STRING LS-LNAME DELIMITED BY SPACE, ', ' DELIMITED BY SIZE, LS-FNAME DELIMITED BY SPACE INTO LS-FULLNAME.</code></li>
                <li>End with <code>EXIT PROGRAM</code>.</li>
            </ol>
        </p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
*> Main Program
IDENTIFICATION DIVISION.
PROGRAM-ID. MAINPROG.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-FNAME      PIC X(10) VALUE 'Mary'.
01 WS-LNAME      PIC X(10) VALUE 'Jones'.
01 WS-FULLNAME   PIC X(22).
PROCEDURE DIVISION.
    CALL 'FORMATNM' USING WS-FNAME, WS-LNAME, WS-FULLNAME.
    DISPLAY 'Formatted Name: ' WS-FULLNAME.
    STOP RUN.

* * * * * * * * * * * * * * * * * * * * * *

*> Subprogram
IDENTIFICATION DIVISION.
PROGRAM-ID. FORMATNM.
DATA DIVISION.
LINKAGE SECTION.
01 LS-FNAME      PIC X(10).
01 LS-LNAME      PIC X(10).
01 LS-FULLNAME   PIC X(22).
PROCEDURE DIVISION USING LS-FNAME, LS-LNAME, LS-FULLNAME.
    STRING LS-LNAME DELIMITED BY SPACE
           ', '     DELIMITED BY SIZE
           LS-FNAME DELIMITED BY SPACE
      INTO LS-FULLNAME.
    EXIT PROGRAM.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "What statement is used in a main program to execute a subprogram?",
            options: [
                "PERFORM",
                "EXECUTE",
                "RUN",
                "CALL"
            ],
            answer: "CALL"
        },
        {
            question: "In a subprogram, where do you define the variables that receive data from the calling program?",
            options: [
                "WORKING-STORAGE SECTION",
                "FILE SECTION",
                "LINKAGE SECTION",
                "INPUT-OUTPUT SECTION"
            ],
            answer: "LINKAGE SECTION"
        },
         {
            question: "Which statement should a subprogram use to return control to the main program?",
            options: [
                "STOP RUN",
                "EXIT PROGRAM",
                "RETURN",
                "END PROGRAM"
            ],
            answer: "EXIT PROGRAM"
        }
    ]
};