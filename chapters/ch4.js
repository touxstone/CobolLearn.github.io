export const content = {
    explanation: `
        <h2>The Heart of COBOL: File Processing</h2>
        <p>COBOL was built for business data processing, which means handling files is one of its most critical features. COBOL has a very structured way of dealing with files, which involves three divisions.</p>
        
        <h2>1. ENVIRONMENT DIVISION: Connecting to the File</h2>
        <p>First, you must link a logical file name used in your program to a physical file on the computer system. This is done in the <code>FILE-CONTROL</code> paragraph using a <code>SELECT</code> statement.</p>
        <pre><code class="language-cobol">
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT STUDENT-FILE ASSIGN TO 'STUDENTS.DAT'.
        </code></pre>
        <p>Here, <code>STUDENT-FILE</code> is the logical name we'll use inside our program, and <code>'STUDENTS.DAT'</code> is the actual name of the file on the disk.</p>

        <h2>2. DATA DIVISION: Describing the File's Structure</h2>
        <p>In the <code>DATA DIVISION</code>, you must add a <code>FILE SECTION</code> to describe the structure of the file's records. This is done with a File Description (<code>FD</code>) entry, followed by a record layout.</p>
        <pre><code class="language-cobol">
DATA DIVISION.
FILE SECTION.
FD STUDENT-FILE.
01 STUDENT-RECORD.
   05 STUDENT-ID      PIC 9(5).
   05 STUDENT-NAME    PIC X(20).
   05 FILLER          PIC X(55). *> Fills the rest of an 80-char record
        </code></pre>

        <h2>3. PROCEDURE DIVISION: Processing the File</h2>
        <p>Finally, you use verbs in the <code>PROCEDURE DIVISION</code> to interact with the file:</p>
        <ul>
            <li><code><strong>OPEN</strong></code>: Prepares a file for use. You can open it as <code>INPUT</code> (read-only), <code>OUTPUT</code> (write-only, creates a new file), <code>I-O</code> (read and write), or <code>EXTEND</code> (append to the end).</li>
            <li><code><strong>READ</strong></code>: Reads the next record from an input file. The <code>AT END</code> clause is crucial for detecting when you've reached the end of the file.</li>
            <li><code><strong>WRITE</strong></code>: Writes a record to an output file.</li>
            <li><code><strong>CLOSE</strong></code>: Releases the file. It is essential to close files when you're done.</li>
        </ul>
        <pre><code class="language-cobol">
PROCEDURE DIVISION.
    OPEN INPUT STUDENT-FILE.
    PERFORM UNTIL END-OF-FILE-FLAG = 'Y'
        READ STUDENT-FILE
            AT END
                MOVE 'Y' TO END-OF-FILE-FLAG
            NOT AT END
                DISPLAY STUDENT-NAME
        END-READ
    END-PERFORM.
    CLOSE STUDENT-FILE.
    STOP RUN.
        </code></pre>
        <p><em>(Note: This example assumes a flag <code>END-OF-FILE-FLAG</code> is defined in <code>WORKING-STORAGE</code>).</em></p>
    `,
    exercise: `
        <h2>Writing to a File</h2>
        <p><strong>Task:</strong> Write a program that creates a new file called 'PRODUCTS.DAT' and writes two product records to it.</p>
        <p><strong>Steps:</strong>
            <ol>
                <li>Set up the <code>SELECT</code> statement for a logical file named <code>PRODUCT-FILE</code>.</li>
                <li>In the <code>FILE SECTION</code>, define an <code>FD</code> for <code>PRODUCT-FILE</code> with a record description <code>PRODUCT-RECORD</code> containing <code>PRODUCT-CODE (PIC 9(4))</code> and <code>PRODUCT-NAME (PIC X(15))</code>.</li>
                <li>In <code>WORKING-STORAGE</code>, create a variable with the same layout as <code>PRODUCT-RECORD</code> (e.g., <code>WS-PRODUCT-OUT</code>) to build your data before writing.</li>
                <li>In the <code>PROCEDURE DIVISION</code>, <code>OPEN</code> the file for <code>OUTPUT</code>.</li>
                <li><code>MOVE</code> data for the first product into <code>WS-PRODUCT-OUT</code> and then <code>WRITE PRODUCT-RECORD FROM WS-PRODUCT-OUT</code>.</li>
                <li>Repeat for the second product.</li>
                <li><code>CLOSE</code> the file.</li>
            </ol>
        </p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. WRITE-PRODUCTS.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT PRODUCT-FILE ASSIGN TO 'PRODUCTS.DAT'.
DATA DIVISION.
FILE SECTION.
FD PRODUCT-FILE.
01 PRODUCT-RECORD.
   05 PRODUCT-CODE      PIC 9(4).
   05 PRODUCT-NAME      PIC X(15).
WORKING-STORAGE SECTION.
01 WS-PRODUCT-OUT.
   05 WS-PRODUCT-CODE   PIC 9(4).
   05 WS-PRODUCT-NAME   PIC X(15).
PROCEDURE DIVISION.
    OPEN OUTPUT PRODUCT-FILE.
    
    MOVE 1001 TO WS-PRODUCT-CODE.
    MOVE 'Laptop' TO WS-PRODUCT-NAME.
    WRITE PRODUCT-RECORD FROM WS-PRODUCT-OUT.
    
    MOVE 1002 TO WS-PRODUCT-CODE.
    MOVE 'Mouse' TO WS-PRODUCT-NAME.
    WRITE PRODUCT-RECORD FROM WS-PRODUCT-OUT.
    
    CLOSE PRODUCT-FILE.
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "Which statement links a program's logical file name to a physical file name?",
            options: [
                "ASSIGN",
                "LINK",
                "CONNECT",
                "SELECT"
            ],
            answer: "SELECT"
        },
        {
            question: "In which section is the File Description (FD) entry for a file defined?",
            options: [
                "WORKING-STORAGE SECTION",
                "FILE SECTION",
                "INPUT-OUTPUT SECTION",
                "PROCEDURE DIVISION"
            ],
            answer: "FILE SECTION"
        },
         {
            question: "Which OPEN mode is used to add new records to the end of an existing file?",
            options: [
                "INPUT",
                "OUTPUT",
                "I-O",
                "EXTEND"
            ],
            answer: "EXTEND"
        }
    ]
};