export const content = {
    explanation: `
        <h2>Beyond Sequential Files</h2>
        <p>Sequential files (Chapter 4) are great for processing every record in order, but they are inefficient if you need to access a specific record directly without reading all the preceding ones. For this, COBOL offers <strong>indexed files</strong>.</p>
        
        <h2>What is an Indexed File?</h2>
        <p>An indexed file is like a book with an index at the back. It's a data file that has a separate index file containing the unique 'key' of each record and its physical location in the data file. This allows the system to jump directly to a specific record.</p>
        
        <h2>Defining an Indexed File</h2>
        <p>Setting up an indexed file involves changes in the <code>ENVIRONMENT DIVISION</code>.</p>
        <pre><code class="language-cobol">
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT CUSTOMER-FILE ASSIGN TO 'CUST.DAT'
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS CUST-ID
        FILE STATUS IS WS-FILE-STATUS.
        </code></pre>
        <ul>
            <li><code><strong>ORGANIZATION IS INDEXED</strong></code>: This is the crucial part that declares the file type.</li>
            <li><code><strong>ACCESS MODE</strong></code>: Can be <code>SEQUENTIAL</code> (read in order), <code>RANDOM</code> (read any record directly), or <code>DYNAMIC</code> (switch between sequential and random).</li>
            <li><code><strong>RECORD KEY</strong></code>: Specifies the field within the record that will be used as the unique key (e.g., Customer ID, Product Code). This field must be part of the file's FD record definition.</li>
            <li><code><strong>FILE STATUS</strong></code>: An optional but highly recommended two-character variable in <code>WORKING-STORAGE</code> that receives a status code after every file I/O operation (e.g., '00' for success, '23' for record not found).</li>
        </ul>

        <h2>Processing an Indexed File</h2>
        <p>With <code>ACCESS MODE IS RANDOM</code>, you can perform new operations:</p>
        <ul>
            <li><code><strong>WRITE</strong></code>: Adds a new record. The system will place it in the correct position based on its <code>RECORD KEY</code>.</li>
            <li><code><strong>READ</strong></code>: To read a specific record, you first <code>MOVE</code> the key you're looking for into the <code>RECORD KEY</code> field, then execute the <code>READ</code>.</li>
            <li><code><strong>REWRITE</strong></code>: Updates an existing record. You must first <code>READ</code> the record successfully before you can <code>REWRITE</code> it.</li>
            <li><code><strong>DELETE</strong></code>: Deletes the record that was most recently read.</li>
        </ul>
        <pre><code class="language-cobol">
*> To read record 12345
MOVE 12345 TO CUST-ID.
READ CUSTOMER-FILE
    INVALID KEY
        DISPLAY 'Record not found!'
END-READ.
        </code></pre>
        <p>The <code>INVALID KEY</code> clause triggers if a <code>READ</code> fails to find the key, or a <code>WRITE</code> attempts to add a key that already exists.</p>
    `,
    exercise: `
        <h2>Creating and Reading an Indexed File</h2>
        <p><strong>Task:</strong> Write a program that creates an indexed file for employees. Write two employee records to it. Then, close the file, re-open it, and randomly read one of the employees back by their ID.</p>
        <p><strong>Steps:</strong>
            <ol>
                <li>In <code>FILE-CONTROL</code>, set up an <code>INDEXED</code> file with <code>ACCESS MODE DYNAMIC</code> and a <code>RECORD KEY</code>.</li>
                <li>In the <code>FILE SECTION</code>, define the employee record, including the key field (e.g., <code>EMP-ID PIC 9(4)</code>).</li>
                <li><strong>Part 1 (Write):</strong> <code>OPEN</code> the file as <code>OUTPUT</code>. <code>MOVE</code> data for two employees into the record buffer and <code>WRITE</code> them. Then <code>CLOSE</code> the file.</li>
                <li><strong>Part 2 (Read):</strong> <code>OPEN</code> the file as <code>INPUT</code>. <code>MOVE</code> the ID of one of the employees you just wrote into the <code>RECORD KEY</code> field.</li>
                <li><code>READ</code> the file. On a successful read, display the employee's data. Use the <code>INVALID KEY</code> clause to handle the case where the ID is not found.</li>
                <li><code>CLOSE</code> the file.</li>
            </ol>
        </p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. INDEXED-IO.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT EMPLOYEE-FILE ASSIGN TO 'EMP.DAT'
        ORGANIZATION IS INDEXED
        ACCESS MODE IS DYNAMIC
        RECORD KEY IS EMP-ID.
DATA DIVISION.
FILE SECTION.
FD EMPLOYEE-FILE.
01 EMPLOYEE-RECORD.
   05 EMP-ID     PIC 9(4).
   05 EMP-NAME   PIC X(20).
PROCEDURE DIVISION.
*> Part 1: Write records
    OPEN OUTPUT EMPLOYEE-FILE.
    MOVE 1001 TO EMP-ID.
    MOVE 'Alice' TO EMP-NAME.
    WRITE EMPLOYEE-RECORD
        INVALID KEY DISPLAY 'WRITE FAILED FOR 1001'.
    MOVE 1002 TO EMP-ID.
    MOVE 'Bob' TO EMP-NAME.
    WRITE EMPLOYEE-RECORD
        INVALID KEY DISPLAY 'WRITE FAILED FOR 1002'.
    CLOSE EMPLOYEE-FILE.

*> Part 2: Randomly read a record
    OPEN INPUT EMPLOYEE-FILE.
    MOVE 1001 TO EMP-ID. *> The key we want to find
    READ EMPLOYEE-FILE
        INVALID KEY
            DISPLAY 'Record 1001 not found.'
        NOT INVALID KEY
            DISPLAY 'Found Record: ' EMP-ID ' - ' EMP-NAME
    END-READ.
    CLOSE EMPLOYEE-FILE.
    
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "What is the main advantage of an indexed file over a sequential file?",
            options: [
                "It takes up less disk space",
                "It allows for direct access to specific records by a key",
                "It can only be written to, making it more secure",
                "It automatically encrypts data"
            ],
            answer: "It allows for direct access to specific records by a key"
        },
        {
            question: "Which `FILE-CONTROL` entry declares the unique field for accessing records in an indexed file?",
            options: [
                "ACCESS KEY IS",
                "UNIQUE ID IS",
                "PRIMARY KEY IS",
                "RECORD KEY IS"
            ],
            answer: "RECORD KEY IS"
        },
         {
            question: "To update an existing record in an indexed file, which two verbs would you typically use in sequence?",
            options: [
                "FIND then CHANGE",
                "MOVE then UPDATE",
                "READ then REWRITE",
                "GET then PUT"
            ],
            answer: "READ then REWRITE"
        }
    ]
};