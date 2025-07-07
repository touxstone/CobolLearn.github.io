export const content = {
    explanation: `
        <h2>The DATA DIVISION</h2>
        <p>To do anything useful, a program needs to store and manipulate data. In COBOL, all data items (variables) must be declared in the <strong>DATA DIVISION</strong> before they can be used.</p>
        <p>The most common section within the DATA DIVISION is the <code>WORKING-STORAGE SECTION.</code>. This is where you define variables that are not part of input or output files.</p>
        
        <h2>Defining Variables with <code>PIC</code></h2>
        <p>Variables are defined with a level number, a variable name, and a <code>PICTURE</code> clause (often shortened to <code>PIC</code>).</p>
        <ul>
            <li><strong>Level Number:</strong> Indicates hierarchy. <code>01</code> is a top-level item. We'll start with just <code>01</code> for now.</li>
            <li><strong>Variable Name:</strong> A user-defined name, like <code>WS-NAME</code>. Prefixes like <code>WS-</code> for Working-Storage are a common and highly recommended convention.</li>
            <li><strong>PICTURE Clause (PIC):</strong> Describes the type and size of the data.</li>
        </ul>

        <h3>Common PIC Types:</h3>
        <ul>
            <li><code>PIC 9</code>: Numeric. <code>PIC 9(4)</code> means a 4-digit number.</li>
            <li><code>PIC X</code>: Alphanumeric (any character). <code>PIC X(10)</code> means a string of 10 characters.</li>
            <li><code>PIC A</code>: Alphabetic (letters and spaces only).</li>
            <li><code>PIC V99</code>: Implied decimal point. <code>PIC 99V99</code> represents a number like 12.34. The 'V' doesn't take up storage space.</li>
        </ul>

        <pre><code class="language-cobol">
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-STUDENT-NAME      PIC X(20).
01 WS-AGE               PIC 9(3).
01 WS-AVERAGE-GRADE     PIC 9V99.
        </code></pre>

        <h2>Using Variables with <code>MOVE</code> and <code>DISPLAY</code></h2>
        <p>The <code>MOVE</code> verb is used to assign a value to a variable. You can then <code>DISPLAY</code> the variable's content.</p>
        <pre><code class="language-cobol">
PROCEDURE DIVISION.
    MOVE 'Alice' TO WS-STUDENT-NAME.
    MOVE 21 TO WS-AGE.
    DISPLAY 'Student: ' WS-STUDENT-NAME.
    DISPLAY 'Age: ' WS-AGE.
    STOP RUN.
        </code></pre>
    `,
    exercise: `
        <h2>Practice with Variables</h2>
        <p><strong>Task:</strong> Write a program that defines two numeric variables, adds them together, and displays the result.
        <ol>
            <li>Define three numeric variables in <code>WORKING-STORAGE</code>: <code>WS-NUM1</code>, <code>WS-NUM2</code>, and <code>WS-RESULT</code>. Make them all <code>PIC 99</code>.</li>
            <li>In the <code>PROCEDURE DIVISION</code>, move the value <code>15</code> to <code>WS-NUM1</code> and <code>20</code> to <code>WS-NUM2</code>.</li>
            <li>Use the <code>ADD</code> verb to add them: <code>ADD WS-NUM1, WS-NUM2 GIVING WS-RESULT.</code></li>
            <li>Display the result with a label, like "The result is: ".</li>
        </ol>
        </p>
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. ADDITION.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-NUM1      PIC 99.
01 WS-NUM2      PIC 99.
01 WS-RESULT    PIC 99.
PROCEDURE DIVISION.
    MOVE 15 TO WS-NUM1.
    MOVE 20 TO WS-NUM2.
    ADD WS-NUM1, WS-NUM2 GIVING WS-RESULT.
    DISPLAY 'The result is: ' WS-RESULT.
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "Where are variables typically defined for general program use?",
            options: [
                "PROCEDURE DIVISION",
                "IDENTIFICATION DIVISION",
                "WORKING-STORAGE SECTION.",
                "ENVIRONMENT DIVISION"
            ],
            answer: "WORKING-STORAGE SECTION."
        },
        {
            question: "What does `PIC X(5)` define?",
            options: [
                "A 5-digit number",
                "A string of exactly 5 characters",
                "A number with 5 decimal places",
                "A variable named X5"
            ],
            answer: "A string of exactly 5 characters"
        },
         {
            question: "Which verb is used to assign a value to a variable?",
            options: [
                "ASSIGN",
                "SET",
                "GIVE",
                "MOVE"
            ],
            answer: "MOVE"
        }
    ]
};