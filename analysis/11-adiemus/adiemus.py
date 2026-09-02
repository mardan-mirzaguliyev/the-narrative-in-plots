from pathlib import Path

from dotenv import load_dotenv
from google import genai

# 1. Load the API key from your Mac's .env file
load_dotenv()

# 2. Initialize the modern GenAI client
client = genai.Client()

# 3. Upload the audio file 
print("Uploading audio file from Mac...")
audio_file = client.files.upload(file="data/adiemus.mp3")
print("Upload complete!")

# 4. Prompt the model
prompt = """
You are a musicologist and phonetician. Listen to this track (Adiemus by Karl Jenkins). 
The lyrics have no dictionary meaning. Describe the emotional progression, timber shifts, 
and vocal layering changes second-by-second. Map out the emotional arc based entirely 
on acoustic texture, rhythmic drive, and choral density.
"""

# 5. Generate content using the current active model
print("Analyzing audio...")
response = client.models.generate_content(
    model="gemini-3.6-flash",    # <-- Updated model string
    contents=[prompt, audio_file]
)

# 6. Output the result
print("\n--- GEMINI AUDIO ANALYSIS ---\n")
print(response.text)

music_scored_gemini = response.text
print(music_scored_gemini)

# 1. Correct the timestamp typo using string replacement
music_scored_gemini = music_scored_gemini.replace("00:19 – 01:56", "01:19 – 01:56")

# 2. Define the desired output file name and path
output_path = Path("data/adiemus_analysis.txt")

# 3. Save the corrected text to the file
output_path.write_text(music_scored_gemini, encoding="utf-8")

print("File successfully saved!")

