export const content = {
    explanation: `
        <h2>Making Decisions with IF</h2>
        <p>Programs need to make decisions. In COBOL, the primary tool for this is the <code>IF</code> statement. The structure is quite readable:</p>
        <pre><code class="language-cobol">
IF condition
    statement-1
    statement-2
ELSE
    statement-3
END-IF.
        </code></pre>
        <p>The <code>ELSE</code> part is optional. The <code>END-IF</code> scope terminator is crucial for nesting <code>IF</code> statements and is considered modern best practice. Conditions can be simple comparisons like <code>WS-AGE > 21</code>, <code>WS-NAME = 'BOB'</code>, or more complex.</p>
        
        <h2>Looping with PERFORM</h2>
        <p>The <code>PERFORM</code> verb is COBOL's main tool for creating loops. It's very versatile.</p>
        <h3>Fixed Number of Loops:</h3>
        <p>To repeat a block of code a specific number of times:</p>
        <pre><code class="language-cobol">
PERFORM 5 TIMES
    DISPLAY 'This is loop iteration...'
END-PERFORM.
        </code></pre>

        <h3>Looping Like a 'for' loop:</h3>
        <p>To iterate with a counter, you use <code>PERFORM VARYING</code>. This is similar to a <code>for</code> loop in other languages.</p>
         <pre><code class="language-cobol">
PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 10
    DISPLAY 'Counter is: ' WS-COUNTER
END-PERFORM.
        </code></pre>
        <p>This code will initialize <code>WS-COUNTER</code> to 1, execute the loop body, increment <code>WS-COUNTER</code> by 1, and continue until the condition <code>WS-COUNTER > 10</code> is true.</p>
    `,
    exercise: `
        <h2>Odd or Even?</h2>
        <p><strong>Task:</strong> Write a program that loops from 1 to 5. For each number, it should display whether the number is odd or even.</p>
        <p><strong>Hints:</strong>
            <ol>
                <li>You'll need a counter variable for the loop (e.g., <code>WS-COUNTER PIC 9</code>) and a variable to hold the remainder (e.g., <code>WS-REMAINDER PIC 9</code>).</li>
                <li>Use <code>PERFORM VARYING</code> to create the loop from 1 to 5.</li>
                <li>Inside the loop, use the <code>DIVIDE</code> verb to check for evenness: <code>DIVIDE WS-COUNTER BY 2 GIVING some-variable REMAINDER WS-REMAINDER.</code></li>
                <li>Use an <code>IF</code> statement to check if <code>WS-REMAINDER</code> is 0. If it is, the number is even, otherwise it's odd.</li>
            </ol>
        </p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. ODD-EVEN.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-COUNTER         PIC 9.
01 WS-TEMP-QUOTIENT   PIC 9.
01 WS-REMAINDER       PIC 9.
PROCEDURE DIVISION.
    PERFORM VARYING WS-COUNTER FROM 1 BY 1 UNTIL WS-COUNTER > 5
        DIVIDE WS-COUNTER BY 2 GIVING WS-TEMP-QUOTIENT
            REMAINDER WS-REMAINDER
        
        IF WS-REMAINDER = 0 THEN
            DISPLAY WS-COUNTER ' is Even'
        ELSE
            DISPLAY WS-COUNTER ' is Odd'
        END-IF
    END-PERFORM.
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "Which statement is used to execute a block of code only if a condition is true?",
            options: [
                "WHEN",
                "CHECK",
                "IF",
                "SELECT"
            ],
            answer: "IF"
        },
        {
            question: "What is the modern, recommended way to end an IF statement block?",
            options: [
                "A period (.) after the last statement",
                "END.",
                "END-IF.",
                "STOP IF."
            ],
            answer: "END-IF."
        },
         {
            question: "How would you write a loop that executes exactly 10 times?",
            options: [
                "LOOP 10 TIMES",
                "PERFORM 10 TIMES",
                "REPEAT 10",
                "DO FROM 1 TO 10"
            ],
            answer: "PERFORM 10 TIMES"
        }
    ]
};