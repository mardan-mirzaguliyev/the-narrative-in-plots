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
def get_or_run_translation(lyrics_raw, music_scored_path, output_csv_path, 
                             output_text_path, id_col="line", text_col="original_text",
                             model="claude-sonnet-5"):
    
    output_csv_path = Path(output_csv_path)
    output_text_path = Path(output_text_path)
    
    # ---- Check cache first ----
    if output_csv_path.exists():
        print(f"Found existing results at: {output_csv_path} — loading from disk.")
        return pd.read_csv(output_csv_path)
    
    print(f"No existing results found at: {output_csv_path} — running translation.")
    
    # 1-2. Load API key and initialize client
    load_dotenv(find_dotenv())
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key:
        raise ValueError("API Key not found! Please check your .env file.")
    client = Anthropic(api_key=api_key)
    
    # 3. Load the audio analysis
    music_scored = Path(music_scored_path).read_text(encoding="utf-8")
    
    # 4. Build the prompt — uses id_col/text_col instead of hardcoded names
    lyrics_text = "\n".join([f"Line {row[id_col]}: {row[text_col]}" for _, row in lyrics_raw.iterrows()])
    
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
    
    # 5. Generate
    print("Translating acoustic emotion to English with Claude...")
    message = client.messages.create(
        model=model,
        max_tokens=3500,
        thinking={"type": "disabled"},
        messages=[{"role": "user", "content": prompt}]
    )
    
    # 6. Extract text
    translation_text = "".join(block.text for block in message.content if block.type == "text")
    
    if not translation_text:
        raise ValueError(f"No text block found. stop_reason: {message.stop_reason}")
    
    output_text_path.write_text(translation_text, encoding="utf-8")
    print(f"Raw translation saved to: {output_text_path}")
    
    # 7. Parse
    rows = [line.split("|", 1) for line in translation_text.strip().split("\n") if "|" in line]
    translation_df = pd.DataFrame(rows, columns=[id_col, "translated_text"])
    translation_df = translation_df[translation_df[id_col].str.isdigit()]
    translation_df[id_col] = translation_df[id_col].astype(int)
    
    # 8. Verify completeness
    expected_lines = set(lyrics_raw[id_col])
    received_lines = set(translation_df[id_col])
    missing_lines = sorted(expected_lines - received_lines)
    if missing_lines:
        print(f"Warning: missing {len(missing_lines)} lines: {missing_lines}")
    
    # 9. Merge — keeps line_id, original_text, and adds translated_text
    adiemus_translated = lyrics_raw.merge(translation_df, on=id_col, how="left")
    
    # 10. Save
    adiemus_translated.to_csv(output_csv_path, index=False)
    print(f"Results saved to: {output_csv_path}")
    
    return adiemus_translated

# Use the function to get the Sonnet 5 translation or read the file from local
adiemus_translated = get_or_run_translation(
    lyrics_raw=lyrics_raw,
    music_scored_path="data/output/adiemus_analysis.txt",
    output_csv_path="data/output/adiemus_translated_full.csv",
    output_text_path="data/output/adiemus_english_translation_sonnet.txt",
    id_col="line_id",
    text_col="original_text"
)

print(adiemus_translated.columns)




