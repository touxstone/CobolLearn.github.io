export const content = {
    explanation: `
        <h2>What is COBOL?</h2>
        <p>COBOL stands for <strong>CO</strong>mmon <strong>B</strong>usiness-<strong>O</strong>riented <strong>L</strong>anguage. It's one of the oldest programming languages, designed in 1959. Despite its age, it is still widely used in legacy systems for finance, insurance, and government.</p>
        <p>COBOL was designed to be human-readable, with an English-like syntax. This makes it verbose but also relatively easy to understand once you grasp its structure.</p>
        
        <h2>Structure of a COBOL Program</h2>
        <p>Every COBOL program is divided into four main parts called <strong>DIVISIONS</strong>. They must appear in this specific order:</p>
        <ol>
            <li><strong>IDENTIFICATION DIVISION.</strong> - Names the program.</li>
            <li><strong>ENVIRONMENT DIVISION.</strong> - Describes the computer environment. (Optional in many modern compilers).</li>
            <li><strong>DATA DIVISION.</strong> - Defines the variables and files the program will use.</li>
            <li><strong>PROCEDURE DIVISION.</strong> - Contains the actual instructions (the logic) of the program.</li>
        </ol>

        <p>Here's a look at the simplest possible COBOL program that does nothing but run successfully:</p>
        <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.
PROCEDURE DIVISION.
    STOP RUN.
        </code></pre>
        <p>Notice that even this tiny program requires two divisions. The <code>PROGRAM-ID</code> is like a name for your program, and <code>STOP RUN</code> is the instruction to end the program. Every statement and division name in COBOL ends with a period.</p>
    `,
    exercise: `
        <h2>Your First Program</h2>
        <p>The most common first program for any language is "Hello, World!". In COBOL, we use the <code>DISPLAY</code> verb to print text to the screen.</p>
        <p><strong>Task:</strong> Modify the basic program structure to display the message "HELLO, COBOL WORLD!".</p>
        <p><strong>Hint:</strong> The <code>DISPLAY</code> statement goes in the <code>PROCEDURE DIVISION</code>. Text you want to display, called a 'literal', must be enclosed in quotes (e.g., <code>'MY TEXT'</code>).</p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLOWORLD.
PROCEDURE DIVISION.
    DISPLAY 'HELLO, COBOL WORLD!'.
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "What does COBOL stand for?",
            options: [
                "Computer Business Object Language",
                "Common Business-Oriented Language",
                "Complex Binary Operation Language",
                "Coded Object Basic Language"
            ],
            answer: "Common Business-Oriented Language"
        },
        {
            question: "Which of the four DIVISIONS is responsible for the program's logic?",
            options: [
                "IDENTIFICATION DIVISION",
                "ENVIRONMENT DIVISION",
                "DATA DIVISION",
                "PROCEDURE DIVISION"
            ],
            answer: "PROCEDURE DIVISION"
        }
    ]
};