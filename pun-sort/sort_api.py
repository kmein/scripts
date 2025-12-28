#!/usr/bin/env python3
"""
FastAPI backend for phonetic word sorting
Sorts words by their phonetic similarity using espeak-ng IPA transcription
"""
from fastapi import FastAPI, HTTPException, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse
from typing import List, Dict, Any
import string
import subprocess
from functools import lru_cache

app = FastAPI(
    title="Phonetic Word Sorter API",
    description="Sort words by phonetic similarity using IPA transcription",
    version="1.0.0"
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# -------------------------
# IPA helpers
# -------------------------
def get_ipa(word: str, lang: str = "en") -> str:
    """Get IPA transcription using espeak-ng"""
    try:
        out = subprocess.check_output(
            ["espeak-ng", "-v", lang, "-q", "--ipa=3", word],
            stderr=subprocess.DEVNULL,
            text=True,
            timeout=5
        )
        return out.strip().strip("/")
    except subprocess.TimeoutExpired:
        raise HTTPException(status_code=504, detail="espeak-ng timeout")
    except FileNotFoundError:
        raise HTTPException(
            status_code=500,
            detail="espeak-ng not found. Please install it: apt-get install espeak-ng"
        )
    except Exception:
        return ""

def ipa_tokenize(ipa: str) -> List[str]:
    """Tokenize IPA string into phonemes"""
    tokens = []
    i = 0
    while i < len(ipa):
        ch = ipa[i]
        if ch in "ˈˌ":
            i += 1
            continue
        if i + 1 < len(ipa) and ipa[i:i+2] in {"aɪ", "aʊ", "eɪ", "oʊ", "ɔɪ"}:
            tokens.append(ipa[i:i+2])
            i += 2
        else:
            tokens.append(ch)
            i += 1
    return tokens

# -------------------------
# Distance calculation
# -------------------------
VOWELS = set("aeiouəɪʊɔɛɜɑæ")

def sub_cost(a: str, b: str) -> float:
    """Calculate substitution cost between two phonemes"""
    if a == b:
        return 0.0
    if a in VOWELS and b in VOWELS:
        return 0.6
    if a in VOWELS or b in VOWELS:
        return 2.0
    return 1.0

@lru_cache(maxsize=None)
def phonetic_distance(a: tuple, b: tuple) -> float:
    """Calculate phonetic edit distance between two IPA token sequences"""
    n, m = len(a), len(b)
    dp = [[0] * (m + 1) for _ in range(n + 1)]

    for i in range(n + 1):
        dp[i][0] = i
    for j in range(m + 1):
        dp[0][j] = j

    for i in range(1, n + 1):
        for j in range(1, m + 1):
            dp[i][j] = min(
                dp[i - 1][j] + 1,
                dp[i][j - 1] + 1,
                dp[i - 1][j - 1] + sub_cost(a[i - 1], b[j - 1])
            )

    return dp[n][m]

def tokenize_text(text: str) -> List[str]:
    """
    Tokenize text into words, removing punctuation.
    Handles Unicode letters (ä, ö, ü, ß, é, ñ, etc.)
    """
    cleaned = text.translate(str.maketrans('', '', string.punctuation))
    tokens = cleaned.split()
    return tokens

# -------------------------
# Seriation algorithm
# -------------------------
def seriate(words: List[str], ipas: dict) -> List[str]:
    """
    Sort words by phonetic similarity using nearest-neighbor seriation
    """
    if len(words) <= 1:
        return words

    unused = set(words)
    path = [words[0]]
    unused.remove(words[0])

    while unused:
        cur = path[-1]
        nxt = min(
            unused,
            key=lambda w: phonetic_distance(ipas[cur], ipas[w]) / max(len(ipas[cur]), len(ipas[w]), 1)
        )
        path.append(nxt)
        unused.remove(nxt)

    return path

# -------------------------
# API Endpoints
# -------------------------
@app.get("/", response_class=HTMLResponse)
async def root():
    """Serve HTML interface"""
    return """
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Phonetic Word Sorter</title>
        <style>
            body {
                font-family: Georgia, serif;
                max-width: 650px;
                margin: 40px auto;
                padding: 0 20px;
                line-height: 1.6;
                color: #222;
            }
            h1 {
                font-size: 1.8em;
                margin-bottom: 0.3em;
                font-weight: normal;
            }
            .subtitle {
                color: #666;
                margin-bottom: 2em;
                font-style: italic;
            }
            label {
                display: block;
                margin-top: 1.5em;
                margin-bottom: 0.3em;
            }
            textarea {
                width: 100%;
                padding: 8px;
                border: 1px solid #ccc;
                font-family: inherit;
                font-size: 1em;
                resize: vertical;
                min-height: 100px;
            }
            select {
                padding: 6px;
                border: 1px solid #ccc;
                font-family: inherit;
                font-size: 1em;
            }
            button {
                margin-top: 1em;
                padding: 8px 16px;
                border: 1px solid #333;
                background: white;
                cursor: pointer;
                font-family: inherit;
                font-size: 1em;
            }
            button:hover {
                background: #f5f5f5;
            }
            button:disabled {
                opacity: 0.5;
                cursor: not-allowed;
            }
            #results {
                margin-top: 2em;
                padding-top: 2em;
                border-top: 1px solid #ddd;
            }
            .result-header {
                margin-bottom: 1em;
                font-weight: normal;
            }
            .stats {
                color: #666;
                font-size: 0.9em;
                margin-bottom: 1.5em;
            }
            .word-item {
                padding: 0.5em 0;
                border-bottom: 1px dotted #ddd;
            }
            .word {
                font-weight: bold;
            }
            .ipa {
                color: #666;
                font-family: monospace;
                margin-left: 1em;
            }
            .error {
                color: #c00;
                margin-top: 1em;
                padding: 1em;
                border-left: 3px solid #c00;
                background: #fff5f5;
            }
        </style>
    </head>
    <body>
        <h1>Phonetic Word Sorter</h1>
        <p class="subtitle">Sort words by their phonetic similarity using IPA transcription</p>

        <label for="text">Enter your text:</label>
        <textarea id="text" placeholder="night knight kite kit bit bite byte">night knight kite kit bit bite byte</textarea>

        <label for="lang">Language:</label>
        <select id="lang">
            <option value="en">English</option>
            <option value="de">German</option>
            <option value="es">Spanish</option>
            <option value="fr">French</option>
            <option value="it">Italian</option>
            <option value="pt">Portuguese</option>
            <option value="nl">Dutch</option>
            <option value="sv">Swedish</option>
            <option value="no">Norwegian</option>
            <option value="da">Danish</option>
        </select>

        <button id="sortBtn" onclick="sortWords()">Sort Words</button>

        <div id="results"></div>

        <script>
            async function sortWords() {
                const text = document.getElementById('text').value;
                const lang = document.getElementById('lang').value;
                const resultsDiv = document.getElementById('results');
                const sortBtn = document.getElementById('sortBtn');

                if (!text.trim()) {
                    resultsDiv.innerHTML = '<div class="error">Please enter some text</div>';
                    return;
                }

                sortBtn.disabled = true;
                sortBtn.textContent = 'Sorting...';
                resultsDiv.innerHTML = '<p>Processing...</p>';

                try {
                    const response = await fetch('/sort', {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json',
                        },
                        body: JSON.stringify({ text, lang })
                    });

                    if (!response.ok) {
                        const error = await response.json();
                        throw new Error(error.detail || 'Request failed');
                    }

                    const data = await response.json();

                    let html = '<h2 class="result-header">Sorted Results</h2>';
                    html += `<div class="stats">${data.original_count} words (${data.unique_count} unique)</div>`;

                    data.sorted_words.forEach(item => {
                        html += `<div class="word-item"><span class="word">${item.word}</span><span class="ipa">/${item.ipa}/</span></div>`;
                    });

                    resultsDiv.innerHTML = html;
                } catch (error) {
                    resultsDiv.innerHTML = `<div class="error">Error: ${error.message}</div>`;
                } finally {
                    sortBtn.disabled = false;
                    sortBtn.textContent = 'Sort Words';
                }
            }

            // Allow Enter key in textarea
            document.getElementById('text').addEventListener('keydown', function(e) {
                if (e.key === 'Enter' && e.ctrlKey) {
                    sortWords();
                }
            });
        </script>
    </body>
    </html>
    """

@app.get("/api", response_class=HTMLResponse)
async def api_info():
    """API information endpoint"""
    return {
        "name": "Phonetic Word Sorter API",
        "version": "1.0.0",
        "endpoints": {
            "GET /": "Web interface",
            "POST /sort": "Sort words by phonetic similarity",
            "POST /ipa": "Get IPA transcription for a single word",
            "GET /health": "Health check"
        }
    }

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    try:
        subprocess.run(
            ["espeak-ng", "--version"],
            capture_output=True,
            timeout=2
        )
        return {"status": "healthy", "espeak_ng": "available"}
    except Exception as e:
        return {"status": "unhealthy", "error": str(e)}

@app.post("/ipa")
async def get_word_ipa(request: Request):
    """
    Get IPA transcription and tokens for a single word

    Request body:
    {
        "word": "hello",
        "lang": "en"
    }
    """
    data = await request.json()

    word = data.get("word")
    if not word:
        raise HTTPException(status_code=400, detail="'word' field is required")

    lang = data.get("lang", "en")

    ipa = get_ipa(word, lang)
    if not ipa:
        raise HTTPException(
            status_code=400,
            detail=f"Could not get IPA for word '{word}'"
        )

    tokens = ipa_tokenize(ipa)

    return {
        "word": word,
        "ipa": ipa,
        "tokens": tokens
    }

@app.post("/sort")
async def sort_words(request: Request):
    """
    Sort words from text by phonetic similarity

    Request body:
    {
        "text": "The quick brown fox jumps over the lazy dog",
        "lang": "en"
    }
    """
    data = await request.json()

    text = data.get("text")
    if not text or not text.strip():
        raise HTTPException(status_code=400, detail="'text' field is required")

    lang = data.get("lang", "en")

    words = tokenize_text(text)

    if not words:
        raise HTTPException(status_code=400, detail="No valid words found in text")

    original_count = len(words)

    seen = set()
    unique_words = []
    for word in words:
        if word not in seen:
            seen.add(word)
            unique_words.append(word)

    ipas = {}
    for word in unique_words:
        ipa = get_ipa(word, lang)
        if ipa:
            ipas[word] = tuple(ipa_tokenize(ipa))
        else:
            ipas[word] = tuple()

    valid_words = [w for w in unique_words if ipas[w]]

    if not valid_words:
        raise HTTPException(
            status_code=400,
            detail="Could not get IPA transcription for any words"
        )

    ordered = seriate(valid_words, ipas)

    sorted_words = [
        {"word": w, "ipa": "".join(ipas[w])}
        for w in ordered
    ]

    return {
        "sorted_words": sorted_words,
        "original_count": original_count,
        "unique_count": len(unique_words)
    }

if __name__ == "__main__":
    import uvicorn
    import os

    port = int(os.environ.get("PORT", 8000))
    uvicorn.run(app, host="0.0.0.0", port=port)
