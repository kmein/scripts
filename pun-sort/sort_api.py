#!/usr/bin/env python3
"""
FastAPI backend for phonetic word sorting
Sorts words by their phonetic similarity using espeak-ng IPA transcription
"""
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
from typing import List, Optional
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
# Models
# -------------------------
class SortRequest(BaseModel):
    text: str = Field(..., description="Text containing words to sort")
    lang: str = Field("en", description="Language code for espeak-ng (e.g., 'en', 'de', 'es')")

    class Config:
        schema_extra = {
            "example": {
                "text": "The quick brown fox jumps over the lazy dog",
                "lang": "en"
            }
        }

class WordIPA(BaseModel):
    word: str
    ipa: str

class SortResponse(BaseModel):
    sorted_words: List[WordIPA]

class IPARequest(BaseModel):
    word: str
    lang: str = Field("en", description="Language code for espeak-ng")

class IPAResponse(BaseModel):
    word: str
    ipa: str
    tokens: List[str]

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
    except Exception as e:
        return ""

def ipa_tokenize(ipa: str) -> List[str]:
    """Tokenize IPA string into phonemes"""
    tokens = []
    i = 0
    while i < len(ipa):
        ch = ipa[i]
        # Skip stress markers
        if ch in "ˈˌ":
            i += 1
            continue
        # Check for diphthongs
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
    # Remove punctuation and split into words
    cleaned = text.translate(str.maketrans('', '', string.punctuation))
    tokens = cleaned.split()
    return [word.lower() for word in tokens]

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
@app.get("/")
def root():
    """Root endpoint with API information"""
    return {
        "name": "Phonetic Word Sorter API",
        "version": "1.0.0",
        "endpoints": {
            "POST /sort": "Sort words by phonetic similarity",
            "POST /ipa": "Get IPA transcription for a single word",
            "GET /health": "Health check"
        }
    }

@app.get("/health")
def health_check():
    """Health check endpoint"""
    try:
        # Test espeak-ng availability
        subprocess.run(
            ["espeak-ng", "--version"],
            capture_output=True,
            timeout=2
        )
        return {"status": "healthy", "espeak_ng": "available"}
    except Exception as e:
        return {"status": "unhealthy", "error": str(e)}

@app.post("/ipa", response_model=IPAResponse)
def get_word_ipa(request: IPARequest):
    """
    Get IPA transcription and tokens for a single word
    """
    ipa = get_ipa(request.word, request.lang)
    if not ipa:
        raise HTTPException(
            status_code=400,
            detail=f"Could not get IPA for word '{request.word}'"
        )

    tokens = ipa_tokenize(ipa)

    return IPAResponse(
        word=request.word,
        ipa=ipa,
        tokens=tokens
    )

@app.post("/sort", response_model=SortResponse)
def sort_words(request: SortRequest):
    """
    Sort words from text by phonetic similarity

    The algorithm:
    1. Tokenizes input text into words
    2. Gets IPA transcription for each word
    3. Tokenizes IPA into phonemes
    4. Uses nearest-neighbor seriation to order words by phonetic similarity
    5. Returns ordered list with IPA transcriptions
    """
    if not request.text.strip():
        raise HTTPException(status_code=400, detail="No text provided")

    # Tokenize text into words
    words = tokenize_text(request.text)

    if not words:
        raise HTTPException(status_code=400, detail="No valid words found in text")

    # Remove duplicates while preserving order
    seen = set()
    unique_words = []
    for word in words:
        if word not in seen:
            seen.add(word)
            unique_words.append(word)

    # Get IPA for all words
    ipas = {}
    for word in unique_words:
        ipa = get_ipa(word, request.lang)
        if ipa:
            ipas[word] = tuple(ipa_tokenize(ipa))
        else:
            # If IPA fails, use empty tuple
            ipas[word] = tuple()

    # Filter out words with no IPA
    valid_words = [w for w in unique_words if ipas[w]]

    if not valid_words:
        raise HTTPException(
            status_code=400,
            detail="Could not get IPA transcription for any words"
        )

    # Sort by phonetic similarity
    ordered = seriate(valid_words, ipas)

    # Build response
    sorted_words = [
        WordIPA(word=w, ipa="".join(ipas[w]))
        for w in ordered
    ]

    return SortResponse(
        sorted_words=sorted_words,
    )

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
