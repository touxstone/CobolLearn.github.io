export const content = {
    explanation: `
        <h2>Powerful Text Handling Verbs</h2>
        <p>Beyond simple <code>MOVE</code> and <code>DISPLAY</code>, COBOL has powerful verbs for detailed text manipulation: <code>INSPECT</code>, <code>STRING</code>, and <code>UNSTRING</code>. These are essential for data validation, cleaning, and formatting.</p>
        
        <h2><code>INSPECT</code>: Counting and Replacing</h2>
        <p>The <code>INSPECT</code> verb examines a string and can count or replace characters.
        <ul>
            <li><strong><code>TALLYING</code></strong>: Counts occurrences of a character or string.</li>
            <li><strong><code>REPLACING</code></strong>: Replaces characters with other characters.</li>
        </ul>
        <pre><code class="language-cobol">
WORKING-STORAGE SECTION.
01 WS-EMAIL      PIC X(30) VALUE 'test-email@example.com'.
01 WS-AT-COUNT   PIC 99 VALUE 0.
...
PROCEDURE DIVISION.
    *> Count how many '@' symbols are in WS-EMAIL
    INSPECT WS-EMAIL TALLYING WS-AT-COUNT FOR ALL '@'.
    *> Result: WS-AT-COUNT is 1

    *> Replace all dashes with underscores
    INSPECT WS-EMAIL REPLACING ALL '-' BY '_'.
    *> Result: WS-EMAIL is now 'test_email@example.com'
        </code></pre>

        <h2><code>STRING</code>: Combining Data</h2>
        <p>The <code>STRING</code> verb concatenates (joins) multiple data items into one. It gives you more control than just displaying items one after another.</p>
        <pre><code class="language-cobol">
01 WS-FNAME      PIC X(10) VALUE 'John'.
01 WS-LNAME      PIC X(10) VALUE 'Doe'.
01 WS-FULL-NAME  PIC X(25).
...
    STRING WS-FNAME   DELIMITED BY SPACE
           ' '        DELIMITED BY SIZE
           WS-LNAME   DELIMITED BY SPACE
      INTO WS-FULL-NAME.
    *> Result: WS-FULL-NAME is 'John Doe' followed by spaces.
        </code></pre>
        <p><code>DELIMITED BY SPACE</code> means it takes characters from the source field until it hits a space. <code>DELIMITED BY SIZE</code> means it takes the entire source field, including spaces.</p>

        <h2><code>UNSTRING</code>: Splitting Data</h2>
        <p><code>UNSTRING</code> is the opposite of <code>STRING</code>. It splits a single data item into multiple pieces based on a delimiter.</p>
        <pre><code class="language-cobol">
01 WS-CSV-DATA   PIC X(30) VALUE '1001,Laptop,1200.50'.
01 WS-PROD-ID    PIC X(4).
01 WS-PROD-NAME  PIC X(10).
01 WS-PROD-PRICE PIC X(8).
...
    UNSTRING WS-CSV-DATA DELIMITED BY ','
        INTO WS-PROD-ID, WS-PROD-NAME, WS-PROD-PRICE.
    *> Result: WS-PROD-ID='1001', WS-PROD-NAME='Laptop', etc.
        </code></pre>
    `,
    exercise: `
        <h2>Parsing Customer Data</h2>
        <p><strong>Task:</strong> You are given a string containing customer information in the format 'LASTNAME;FIRSTNAME;CITY'. Parse this string into three separate variables and display them.</p>
        <p><strong>Steps:</strong>
            <ol>
                <li>In <code>WORKING-STORAGE</code>, define a source string variable (e.g., <code>WS-CUSTOMER-IN</code>) and give it a value like 'SMITH;JANE;LONDON'.</li>
                <li>Define three destination variables: <code>WS-CUST-LNAME</code>, <code>WS-CUST-FNAME</code>, and <code>WS-CUST-CITY</code>, with appropriate <code>PIC</code> clauses.</li>
                <li>Use the <code>UNSTRING</code> verb to parse <code>WS-CUSTOMER-IN</code>. The delimiter is the semicolon (';').</li>
                <li>Display each of the three new variables with appropriate labels.</li>
            </ol>
        </p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. PARSE-CUSTOMER.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-CUSTOMER-IN    PIC X(40) VALUE 'WILLIAMS;PETER;NEW YORK'.
01 WS-CUST-LNAME     PIC X(15).
01 WS-CUST-FNAME     PIC X(15).
01 WS-CUST-CITY      PIC X(15).
PROCEDURE DIVISION.
    UNSTRING WS-CUSTOMER-IN DELIMITED BY ';'
        INTO WS-CUST-LNAME, WS-CUST-FNAME, WS-CUST-CITY.
    
    DISPLAY 'Last Name: ' WS-CUST-LNAME.
    DISPLAY 'First Name: ' WS-CUST-FNAME.
    DISPLAY 'City: ' WS-CUST-CITY.
    
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "Which verb would you use to count how many times the letter 'A' appears in a variable?",
            options: [
                "COUNT",
                "STRING",
                "INSPECT...TALLYING",
                "UNSTRING"
            ],
            answer: "INSPECT...TALLYING"
        },
        {
            question: "What is the primary purpose of the STRING verb?",
            options: [
                "To split one field into many",
                "To define a string variable",
                "To remove spaces from a field",
                "To combine multiple fields into one"
            ],
            answer: "To combine multiple fields into one"
        },
         {
            question: "If you have a variable containing 'USA-NY-NYC', which verb would you use to separate it into three variables for country, state, and city?",
            options: [
                "DIVIDE",
                "UNSTRING",
                "INSPECT",
                "SEPARATE"
            ],
            answer: "UNSTRING"
        }
    ]
};