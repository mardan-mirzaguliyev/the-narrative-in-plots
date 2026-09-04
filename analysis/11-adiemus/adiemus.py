import os
from pathlib import Path

import pandas as pd
from anthropic import Anthropic
from dotenv import find_dotenv, load_dotenv
from google import genai

# Define the path to your Excel file inside your data folder
excel_path = Path("data/input/adiemus.xlsx")

# Read a specific sheet into a pandas DataFrame
lyrics_raw = pd.read_excel(excel_path, sheet_name="Adiemus")

# Preview the first few rows
print(lyrics_raw.head())


# Gemini 3.6 Flash pipeline

# 1. Load environment and initialize client
load_dotenv()
client = genai.Client()

# 2. Define paths using pathlib
output_path = Path("data/output/adiemus_analysis_gemini.txt")
audio_path = Path("data/input/adiemus.mp3")

# 3. Conditional run: Check if the clean analysis file already exists locally
if output_path.exists():
    print("Local analysis file found. Skipping API call and loading from disk...")
    music_scored_gemini = output_path.read_text(encoding="utf-8")
else:
    print("Local file not found. Uploading audio file...")
    
    # Upload the file using the Files API client
    audio_file = client.files.upload(file=audio_path)
    print("Upload complete!")

    # Define the prompt
    prompt = """
    You are a musicologist and phonetician. Listen to this track (Adiemus by Karl Jenkins). 
    The lyrics have no dictionary meaning. Describe the emotional progression, timber shifts, 
    and vocal layering changes second-by-second. Map out the emotional arc based entirely 
    on acoustic texture, rhythmic drive, and choral density.
    """

    # Generate content using client.chats for robust handling
    print("Analyzing audio...")
    chat = client.chats.create(model="gemini-3.6-flash")
    response = chat.send_message([prompt, audio_file])

    music_scored_gemini = response.text

    # Ensure the parent directory exists, then save the clean text directly
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(music_scored_gemini, encoding="utf-8")
    
    print("API analysis completed and successfully saved to disk!")

# 4. Preview the loaded or newly generated text
print("\n--- ADIEMUS ANALYSIS PREVIEW ---\n")
print(music_scored_gemini[:500] + "...\n")


# Claude Sonnet 5 pipeline
# 1. Load the API key from your Mac's .env file
load_dotenv(find_dotenv())

# 2. Initialize the Anthropic client
api_key = os.environ.get("ANTHROPIC_API_KEY")
if not api_key:
    raise ValueError("API Key not found! Please check your .env file.")

client = Anthropic(api_key=api_key)

# 3. Load your corrected audio analysis from earlier
music_scored = Path("data/output/adiemus_analysis.txt").read_text(encoding="utf-8")

# 4. Engineer the Translation Prompt — output format is now explicit and strict,
#    so parsing doesn't depend on guessing Claude's markdown choices each run.
lyrics_text = "\n".join([f"Line {row['line']}: {row['text']}" for _, row in lyrics_raw.iterrows()])

prompt = f"""
You are a master poet and linguist. I am providing you with two elements:
1. The pseudo-language lyrics of Karl Jenkins' "Adiemus" (which have no dictionary meaning).
2. A structural analysis of the track's acoustic tension and emotional arc.

Your task is to "translate" these lyrics into English. Because there is no literal translation, you must translate the EMOTION.
- Match the rhythmic cadence and syllable count of the original lines as closely as possible.
- Ensure the imagery shifts dynamically to match the emotional arc described in the analysis (e.g., transition from "pastoral serenity" to "exuberant triumph").

Output format is strict and must be followed exactly, with no markdown formatting, no header row, no column labels, and no preamble or explanation — begin your response immediately with the first data line:
LINE_NUMBER|TRANSLATED_TEXT

One line per entry, pipe-separated, nothing else in the response.

Here is the emotional roadmap:
{music_scored}

Here are the lyrics to translate:
{lyrics_text}
"""

# 5. Generate the English interpretation
print("Translating acoustic emotion to English with Claude...")
message = client.messages.create(
    model="claude-sonnet-5",
    max_tokens=3500,
    thinking={"type": "disabled"},
    messages=[
        {"role": "user", "content": prompt}
    ]
)

# 6. Extract the text safely — must happen before any parsing step
translation_text = "".join(block.text for block in message.content if block.type == "text")

if not translation_text:
    print(f"Warning: No text block found. stop_reason: {message.stop_reason}")
    print("Printing full message content for inspection:")
    print(message.content)
else:
    output_path = Path("data/output/adiemus_english_translation_sonnet.txt")
    output_path.write_text(translation_text, encoding="utf-8")
    print("\nTranslation complete and saved!")
    print("\n--- CLAUDE'S PREVIEW ---")
    print(translation_text[:500] + "...\n")

# 7. Parse the pipe-delimited output — robust regardless of any stylistic
rows = [line.split("|", 1) for line in translation_text.strip().split("\n") if "|" in line]
translation_df = pd.DataFrame(rows, columns=["line", "translated_text"])

# Drop the header row Claude echoed back, keep only genuinely numeric line entries
translation_df = translation_df[translation_df["line"].str.isdigit()]
translation_df["line"] = translation_df["line"].astype(int)

print(translation_df)

# 8. Confirm nothing's missing before treating this as final
expected_lines = set(lyrics_raw["line"])
received_lines = set(translation_df["line"])
missing_lines = sorted(expected_lines - received_lines)
print(f"Missing {len(missing_lines)} lines: {missing_lines}")

# 9. Merge with the original source for a full side-by-side comparison
adiemus_translated = lyrics_raw.merge(translation_df, on="line", how="left")
print(adiemus_translated)

# 10. Save the final, verified table
adiemus_translated.to_csv("data/output/adiemus_translated_full.csv", index=False)



