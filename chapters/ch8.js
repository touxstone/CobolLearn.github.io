export const content = {
    explanation: `
        <h2>Finding Data in Tables</h2>
        <p>Loading data into tables is useful, but the real power comes from being able to find specific information quickly. COBOL provides two primary methods for this: a sequential search (<code>SEARCH</code>) and a binary search (<code>SEARCH ALL</code>).</p>
        
        <h2>Indexes vs. Subscripts</h2>
        <p>To use the <code>SEARCH</code> verb, you must define an <strong>index</strong> for your table using the <code>INDEXED BY</code> clause. An index is similar to a subscript, but it's a special pointer managed by COBOL for high-speed table operations. You manipulate indexes with the <code>SET</code> verb.</p>
        
        <h2>Sequential Search: <code>SEARCH</code></h2>
        <p>A sequential search checks each element of the table one by one, starting from the beginning. It's simple but can be slow for large tables.</p>
        <p>Before starting the search, you must <code>SET</code> the index to 1.</p>
        <pre><code class="language-cobol">
01 COUNTRY-TABLE.
   05 COUNTRY-ENTRY OCCURS 10 TIMES INDEXED BY I-COUNTRY.
      10 COUNTRY-CODE  PIC XX.
      10 COUNTRY-NAME  PIC X(20).
...
PROCEDURE DIVISION.
    SET I-COUNTRY TO 1. *> Start search from the first element
    SEARCH COUNTRY-ENTRY
        AT END
            DISPLAY 'Country not found'
        WHEN COUNTRY-CODE (I-COUNTRY) = 'JP'
            DISPLAY 'Found: ' COUNTRY-NAME (I-COUNTRY)
    END-SEARCH.
        </code></pre>
        
        <h2>Binary Search: <code>SEARCH ALL</code></h2>
        <p>A binary search is much faster but has a critical requirement: <strong>the table must be sorted</strong>. You declare this in the table definition using <code>ASCENDING KEY</code> or <code>DESCENDING KEY</code>.</p>
        <p><code>SEARCH ALL</code> automatically and efficiently finds the data. You do not need to <code>SET</code> the index before the search.</p>
        <pre><code class="language-cobol">
01 PRODUCT-TABLE.
   05 PRODUCT-ENTRY OCCURS 100 TIMES
      ASCENDING KEY IS PROD-ID
      INDEXED BY I-PROD.
      10 PROD-ID    PIC 9(4).
      10 PROD-DESC  PIC X(30).
...
PROCEDURE DIVISION.
    SEARCH ALL PRODUCT-ENTRY
        AT END
            DISPLAY 'Product not in table'
        WHEN PROD-ID (I-PROD) = WS-SEARCH-ID
            DISPLAY 'Description: ' PROD-DESC (I-PROD)
    END-SEARCH.
        </code></pre>
    `,
    exercise: `
        <h2>Find a State Capital</h2>
        <p><strong>Task:</strong> Create a program with a table containing U.S. states and their capitals. The table should be sorted by state name. Prompt the user for a state, and use <code>SEARCH ALL</code> to find and display its capital.</p>
        <p><strong>Steps:</strong>
            <ol>
                <li>Define a table in <code>WORKING-STORAGE</code> that <code>OCCURS</code> several times. It should be sorted (<code>ASCENDING KEY</code>) on the state name and be <code>INDEXED BY</code> an index variable. Each entry should have a state name and a capital name.</li>
                <li>Populate the table with a few state/capital pairs (e.g., California/Sacramento, Texas/Austin, Florida/Tallahassee) using <code>MOVE</code> statements. Make sure they are in alphabetical order by state name.</li>
                <li>Define a variable to hold the user's input, e.g., <code>WS-INPUT-STATE</code>. For this exercise, you can just <code>MOVE</code> a value to it instead of accepting real input.</li>
                <li>Use <code>SEARCH ALL</code> on your table. In the <code>WHEN</code> clause, compare the table's state name with <code>WS-INPUT-STATE</code>.</li>
                <li>If found, display the corresponding capital. If not found (<code>AT END</code>), display a "State not found" message.</li>
            </ol>
        </p>
        
        <div class="exercise-solution">
            <button id="show-solution-btn">Show Solution</button>
            <div id="solution-code">
                <pre><code class="language-cobol">
IDENTIFICATION DIVISION.
PROGRAM-ID. SEARCH-CAPITAL.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-INPUT-STATE       PIC X(15) VALUE 'Texas'.
01 STATE-TABLE.
   05 STATE-DATA OCCURS 3 TIMES
      ASCENDING KEY IS T-STATE
      INDEXED BY I-STATE.
      10 T-STATE    PIC X(15).
      10 T-CAPITAL  PIC X(15).
PROCEDURE DIVISION.
    MOVE 'California' TO T-STATE(1).
    MOVE 'Sacramento' TO T-CAPITAL(1).
    MOVE 'Florida'    TO T-STATE(2).
    MOVE 'Tallahassee'TO T-CAPITAL(2).
    MOVE 'Texas'      TO T-STATE(3).
    MOVE 'Austin'     TO T-CAPITAL(3).

    SEARCH ALL STATE-DATA
        AT END
            DISPLAY 'State not found.'
        WHEN T-STATE(I-STATE) = WS-INPUT-STATE
            DISPLAY 'The capital of ' T-STATE(I-STATE)
                    ' is ' T-CAPITAL(I-STATE) '.'.
    END-SEARCH.
    
    STOP RUN.
                </code></pre>
            </div>
        </div>
    `,
    test: [
        {
            question: "What is the main requirement for using the `SEARCH ALL` verb?",
            options: [
                "The table must have an odd number of elements",
                "The table must be sorted",
                "The table index must be set to 1 before the search",
                "The table must not contain duplicate values"
            ],
            answer: "The table must be sorted"
        },
        {
            question: "Which clause is used in a table definition to prepare it for a binary search?",
            options: [
                "ORDER BY",
                "SORTED BY",
                "ASCENDING KEY IS",
                "BINARY SEARCH IS"
            ],
            answer: "ASCENDING KEY IS"
        },
         {
            question: "Which verb is used to initialize or change the value of an index variable?",
            options: [
                "MOVE",
                "COMPUTE",
                "INITIALIZE",
                "SET"
            ],
            answer: "SET"
        }
    ]
};