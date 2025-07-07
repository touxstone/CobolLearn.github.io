export const content = {
    explanation: `
        <h2>Defining Groups of Data</h2>
        <p>Often, you need to store and process a list of similar data items, like monthly sales figures or a list of student names. In COBOL, these lists are called <strong>tables</strong>, which are equivalent to arrays in other languages.</p>
        
        <h2>The <code>OCCURS</code> Clause</h2>
        <p>You define a table using the <code>OCCURS</code> clause in the <code>DATA DIVISION</code>. This clause specifies how many times a data item definition is repeated.</p>
        
        <h3>One-Dimensional Table</h3>
        <p>Here's how to define a table to hold sales figures for all 12 months:</p>
        <pre><code class="language-cobol">
DATA DIVISION.
WORKING-STORAGE SECTION.
01 SALES-DATA.
   05 MONTHLY-SALES      PIC 9(5)V99 OCCURS 12 TIMES.
        </code></pre>
        <p>This creates 12 slots for <code>MONTHLY-SALES</code>. To access a specific month's sales, you use a <strong>subscript</strong> (an index) in parentheses. For example, <code>MONTHLY-SALES (3)</code> refers to the third month's sales (March).</p>
        <p><strong>Important:</strong> Subscripts in COBOL are 1-based, not 0-based!</p>

        <h3>Populating and Reading a Table</h3>
        <p>The <code>PERFORM VARYING</code> loop is perfect for working with tables.</p>
        <pre><code class="language-cobol">
DATA DIVISION.
WORKING-STORAGE SECTION.
01 SALES-DATA.
   05 MONTHLY-SALES      PIC 9(5) OCCURS 12 TIMES.
01 WS-MONTH-INDEX       PIC 99.
...
PROCEDURE DIVISION.
    *> Populate the table (example)
    PERFORM VARYING WS-MONTH-INDEX FROM 1 BY 1 UNTIL WS-MONTH-INDEX > 12
        MOVE WS-MONTH-INDEX * 100 TO MONTHLY-SALES(WS-MONTH-INDEX)
    END-PERFORM.

    *> Display the value for June (the 6th element)
    DISPLAY 'June sales: ' MONTHLY-SALES(6).
        </code></pre>

        <h2>Two-Dimensional Tables</h2>
        <p>You can create multi-dimensional tables by nesting <code>OCCURS</code> clauses. For example, a table to store test scores for 20 students in 5 subjects:</p>
        <pre><code class="language-cobol">
01 STUDENT-SCORES-TABLE.
   05 STUDENT-DATA       OCCURS 20 TIMES.
      10 STUDENT-NAME    PIC X(30).
      10 TEST-SCORES     PIC 9(3) OCCURS 5 TIMES.
        </code></pre>
        <p>To access the 3rd test score for the 10th student, you would use <code>TEST-SCORES (10, 3)</code>.</p>
    `,
    exercise: `
        <h2>Calculating an Average</h2>
        <p><strong>Task:</strong> Create a program that defines a table of 5 integers, populates it with some values, and then calculates and displays the average of those values.</p>
        <p><strong>Steps:</strong>
            <ol>
                <li>In <code>WORKING-STORAGE</code>, define a table named <code>NUMBER-TABLE</code> that <code>OCCURS 5 TIMES</code> with <code>PIC 99</code>.</li>
                <li>Define a variable for the loop index (e.g., <code>WS-INDEX PIC 9</code>), a variable for the sum (e.g., <code>WS-SUM PIC 9(3)</code>), and a variable for the average (e.g., <code>WS-AVERAGE PIC 99V9</code>).</li>
                <li>Populate the table with five numbers of your choice (e.g., 10, 20, 30, 40, 50). You can do this with individual <code>MOVE</code> statements.</li>
                <li>Use a <code>PERFORM VARYING</code> loop to iterate from 1 to 5. Inside the loop, add the current table element to <code>WS-SUM</code>.</li>
                <li>After the loop, use <code>DIVIDE WS-SUM BY 5 GIVING WS-AVERAGE</code>.</li>
                <li><code>DISPLAY</code> the calculated average.</li>
            </ol>
        </p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. AVERAGE-CALC.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 NUMBER-TABLE.
   05 NUM-ITEM       PIC 99 OCCURS 5 TIMES.
01 WS-INDEX         PIC 9.
01 WS-SUM           PIC 9(3) VALUE 0.
01 WS-AVERAGE       PIC 99V9.
PROCEDURE DIVISION.
    MOVE 10 TO NUM-ITEM(1).
    MOVE 22 TO NUM-ITEM(2).
    MOVE 35 TO NUM-ITEM(3).
    MOVE 41 TO NUM-ITEM(4).
    MOVE 59 TO NUM-ITEM(5).
    
    PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
        ADD NUM-ITEM(WS-INDEX) TO WS-SUM
    END-PERFORM.
    
    DIVIDE WS-SUM BY 5 GIVING WS-AVERAGE.
    
    DISPLAY 'The average is: ' WS-AVERAGE.
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "Which clause is used to define a table (an array) in COBOL?",
            options: [
                "TABLE",
                "ARRAY",
                "LIST",
                "OCCURS"
            ],
            answer: "OCCURS"
        },
        {
            question: "If a table is defined as `05 MY-ITEM PIC X(4) OCCURS 10 TIMES.`, how do you access the seventh element?",
            options: [
                "MY-ITEM[7]",
                "MY-ITEM(7)",
                "MY-ITEM[6]",
                "MY-ITEM(6)"
            ],
            answer: "MY-ITEM(7)"
        },
         {
            question: "What is the most suitable verb for iterating through all elements of a table?",
            options: [
                "IF VARYING",
                "LOOP UNTIL",
                "PERFORM VARYING",
                "REPEAT FOR"
            ],
            answer: "PERFORM VARYING"
        }
    ]
};