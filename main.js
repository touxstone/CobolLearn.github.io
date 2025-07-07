import { courseManifest } from './courseData.js';
import hljs from 'highlight.js';

const state = {
    chapters: courseManifest, // Now loading manifest
    currentChapterIndex: -1,
    progress: {} // { ch1: true, ch2: false, ... }
};

// DOM Elements
const chapterListEl = document.getElementById('chapter-list');
const contentContainerEl = document.getElementById('content-container');
const progressBarEl = document.getElementById('progress-bar');
const progressTextEl = document.getElementById('progress-text');
const prevBtn = document.getElementById('prev-btn');
const nextBtn = document.getElementById('next-btn');
const menuToggleBtn = document.getElementById('menu-toggle-btn');
const sidebarEl = document.getElementById('sidebar');

function navigateToWelcomeScreen() {
    state.currentChapterIndex = -1;
    localStorage.removeItem('cobolLearnLastChapter');
    renderAll();
}

function setupLogoLinks() {
    const logos = document.querySelectorAll('.logo a');
    logos.forEach(logo => {
        logo.addEventListener('click', (e) => {
            e.preventDefault();
            navigateToWelcomeScreen();

            // If on mobile and sidebar is open, close it.
            if (window.innerWidth <= 800 && document.body.classList.contains('sidebar-open')) {
                document.body.classList.remove('sidebar-open');
                menuToggleBtn.setAttribute('aria-expanded', 'false');
            }
        });
    });
}

function setupMobileMenu() {
    // Create and append overlay
    const overlay = document.createElement('div');
    overlay.className = 'sidebar-overlay';
    document.body.appendChild(overlay);

    const toggleSidebar = () => {
        const isOpen = document.body.classList.toggle('sidebar-open');
        menuToggleBtn.setAttribute('aria-expanded', isOpen);
    };

    menuToggleBtn.addEventListener('click', toggleSidebar);
    overlay.addEventListener('click', toggleSidebar);

    // Close sidebar when a chapter is clicked on mobile
    sidebarEl.addEventListener('click', (e) => {
        if (window.innerWidth <= 800 && e.target.closest('.chapter-item')) {
            toggleSidebar();
        }
    });
}

function saveProgress() {
    localStorage.setItem('cobolLearnProgress', JSON.stringify(state.progress));
}

function loadProgress() {
    const savedProgress = localStorage.getItem('cobolLearnProgress');
    if (savedProgress) {
        state.progress = JSON.parse(savedProgress);
    } else {
        // Initialize progress from the manifest
        state.chapters.forEach(ch => {
            state.progress[ch.id] = false;
        });
    }

    // Load last viewed chapter
    const lastChapterId = localStorage.getItem('cobolLearnLastChapter');
    if (lastChapterId) {
        const chapterIndex = state.chapters.findIndex(ch => ch.id === lastChapterId);
        if (chapterIndex > -1) {
            state.currentChapterIndex = chapterIndex;
        }
    }
}

function updateProgressTracker() {
    const completedChapters = Object.values(state.progress).filter(Boolean).length;
    const totalChapters = state.chapters.length;
    const percentage = totalChapters > 0 ? (completedChapters / totalChapters) * 100 : 0;

    progressBarEl.style.width = `${percentage}%`;
    progressTextEl.textContent = `${Math.round(percentage)}% Complete (${completedChapters}/${totalChapters})`;
}

function renderSidebar() {
    chapterListEl.innerHTML = '';

    // Add Welcome/Home link
    const welcomeLi = document.createElement('li');
    welcomeLi.className = 'chapter-item';
    if (state.currentChapterIndex === -1) {
        welcomeLi.classList.add('active');
    }
    welcomeLi.innerHTML = `
        <span class="chapter-number" style="background-color: var(--secondary-accent); color: var(--bg-dark);">🏠</span>
        <span class="chapter-title">Welcome</span>
    `;
    welcomeLi.addEventListener('click', () => {
        navigateToWelcomeScreen();
    });
    chapterListEl.appendChild(welcomeLi);

    state.chapters.forEach((chapter, index) => {
        const li = document.createElement('li');
        li.className = 'chapter-item';
        li.dataset.index = index;
        li.innerHTML = `
            <span class="chapter-number">${index + 1}</span>
            <span class="chapter-title">${chapter.title}</span>
        `;

        if (index === state.currentChapterIndex) {
            li.classList.add('active');
        }
        if (state.progress[chapter.id]) {
            li.classList.add('completed');
        }

        li.addEventListener('click', () => {
            navigateToChapter(index);
        });
        chapterListEl.appendChild(li);
    });
}

function renderWelcomeScreen() {
    contentContainerEl.innerHTML = `
        <div class="content-header"><h1>Welcome to COBOLLearn!</h1></div>
        <p>This is an interactive course designed to teach you the fundamentals of COBOL, a language that has been a cornerstone of business, finance, and administrative systems for decades.</p>
        <p>Please select a chapter from the left sidebar to begin, or use the course overview below to jump to a specific topic.</p>
        
        <div id="course-overview">
            <h2>Course Overview</h2>
            <ul id="course-index-list">
                ${state.chapters.map((chapter, index) => `
                    <li><a href="#" data-index="${index}">${index + 1}. ${chapter.title}</a></li>
                `).join('')}
            </ul>
        </div>

        <div id="references-box">
            <h2>Public Domain & Open Resources</h2>
            <p>The content of this course is inspired by and adapted from various educational materials. For further reading, we recommend these excellent resources:</p>
            <ul>
                <li><a href="https://www.ibm.com/docs/en/cobol-zos/6.4?topic=cobol-language-reference" target="_blank" rel="noopener noreferrer">IBM COBOL Language Reference</a> - An extensive and official reference manual.</li>
                <li><a href="https://open-cobol.sourceforge.io/guides/GnuCOBOL-Programmers-Guide.pdf" target="_blank" rel="noopener noreferrer">GnuCOBOL Programmer's Guide (PDF)</a> - A comprehensive guide for the open-source GnuCOBOL compiler.</li>
                <li>Various public-domain tutorials and code examples from academic institutions and programming communities.</li>
            </ul>
        </div>
    `;

    contentContainerEl.querySelectorAll('#course-index-list a').forEach(link => {
        link.addEventListener('click', (e) => {
            e.preventDefault();
            const index = parseInt(e.target.dataset.index, 10);
            navigateToChapter(index);
        });
    });
}

async function renderContent() {
    if (state.currentChapterIndex === -1) {
        renderWelcomeScreen();
        updateNavButtons();
        return;
    }

    const chapter = state.chapters[state.currentChapterIndex];
    if (!chapter) return;

    // Dynamically load content if it's not already loaded
    if (!chapter.content) {
        try {
            const module = await import(chapter.path);
            chapter.content = module.content;
        } catch (error) {
            console.error("Failed to load chapter content:", error);
            contentContainerEl.innerHTML = `<p>Error: Could not load chapter content. Please try again later.</p>`;
            return;
        }
    }

    localStorage.setItem('cobolLearnLastChapter', chapter.id);

    contentContainerEl.innerHTML = `
        <div class="content-header">
            <h1>${chapter.title}</h1>
            <div class="content-tabs">
                <button class="tab-btn active" data-tab="explanation">Explanation</button>
                <button class="tab-btn" data-tab="exercise">Exercise</button>
                <button class="tab-btn" data-tab="test">Test</button>
            </div>
        </div>
        <div id="explanation-content" class="tab-content active">
            ${chapter.content.explanation}
        </div>
        <div id="exercise-content" class="tab-content">
            ${chapter.content.exercise}
        </div>
        <div id="test-content" class="tab-content">
            ${renderQuiz(chapter)}
        </div>
    `;

    contentContainerEl.querySelectorAll('pre code').forEach((block) => {
        hljs.highlightElement(block);
    });

    addContentEventListeners();
    updateNavButtons();
}

function renderQuiz(chapter) {
    if (!chapter.content || !chapter.content.test || chapter.content.test.length === 0) {
        return `<p>No test available for this chapter.</p>`;
    }

    const questionsHTML = chapter.content.test.map((q, index) => {
        const optionsHTML = q.options.map((option, i) => `
            <li class="quiz-option">
                <input type="radio" id="q${index}_option${i}" name="question${index}" value="${option}">
                <label for="q${index}_option${i}">${option}</label>
            </li>
        `).join('');
        return `
            <div class="quiz-question">
                <p><strong>${index + 1}. ${q.question}</strong></p>
                <ul class="quiz-options">${optionsHTML}</ul>
            </div>
        `;
    }).join('');

    return `
        <div class="quiz-container">
            <h2>Chapter Test</h2>
            <p>Get all questions right to mark this chapter as complete.</p>
            <form id="quiz-form">
                ${questionsHTML}
                <button type="submit" id="submit-quiz-btn">Submit Answers</button>
            </form>
            <div id="quiz-result"></div>
        </div>
    `;
}

function addContentEventListeners() {
    const tabs = contentContainerEl.querySelectorAll('.tab-btn');
    tabs.forEach(tab => {
        tab.addEventListener('click', () => {
            tabs.forEach(t => t.classList.remove('active'));
            tab.classList.add('active');

            contentContainerEl.querySelectorAll('.tab-content').forEach(content => {
                content.classList.remove('active');
            });
            document.getElementById(`${tab.dataset.tab}-content`).classList.add('active');
        });
    });
    
    const showSolutionBtn = document.getElementById('show-solution-btn');
    if (showSolutionBtn) {
        showSolutionBtn.addEventListener('click', () => {
            const solutionCode = document.getElementById('solution-code');
            solutionCode.style.display = 'block';
            showSolutionBtn.style.display = 'none';
        });
    }

    const quizForm = document.getElementById('quiz-form');
    if(quizForm) {
        quizForm.addEventListener('submit', handleQuizSubmit);
    }
}

function handleQuizSubmit(e) {
    e.preventDefault();
    const chapter = state.chapters[state.currentChapterIndex];
    const questions = chapter.content.test;
    let score = 0;
    
    questions.forEach((q, index) => {
        const selectedOption = document.querySelector(`input[name="question${index}"]:checked`);
        const optionLabels = document.querySelectorAll(`input[name="question${index}"] + label`);
        optionLabels.forEach(l => {
            l.classList.remove('correct', 'incorrect');
        });

        if (selectedOption) {
            const label = selectedOption.nextElementSibling;
            if (selectedOption.value === q.answer) {
                score++;
                label.classList.add('correct');
            } else {
                label.classList.add('incorrect');
            }
        }
    });

    quizForm.querySelectorAll('input[type="radio"]').forEach(radio => radio.disabled = true);
    document.getElementById('submit-quiz-btn').disabled = true;

    const resultEl = document.getElementById('quiz-result');
    if (score === questions.length) {
        resultEl.textContent = `Perfect! You got ${score}/${questions.length} correct. Chapter complete!`;
        resultEl.className = 'correct';
        state.progress[chapter.id] = true;
        saveProgress();
        updateProgressTracker();
        renderSidebar();
    } else {
        resultEl.textContent = `You got ${score}/${questions.length}. Review the material and try again! Your correct/incorrect answers are highlighted above.`;
        resultEl.className = 'incorrect';
    }
}

function updateNavButtons() {
    prevBtn.disabled = state.currentChapterIndex === -1;
    nextBtn.disabled = state.currentChapterIndex === -1 || state.currentChapterIndex >= state.chapters.length - 1;
}

function navigateToChapter(index) {
    if (index < 0 || index >= state.chapters.length) return;
    state.currentChapterIndex = index;
    renderAll();
    contentContainerEl.parentNode.scrollTop = 0; // scroll to top
}

function renderAll() {
    renderSidebar();
    renderContent();
    updateProgressTracker();
}

function init() {
    loadProgress();
    renderAll();
    setupMobileMenu();
    setupLogoLinks();

    prevBtn.addEventListener('click', () => {
        navigateToChapter(state.currentChapterIndex - 1);
    });
    nextBtn.addEventListener('click', () => {
        navigateToChapter(state.currentChapterIndex + 1);
    });
}

init();