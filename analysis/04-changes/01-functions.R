# Local LLM connection

### phi4-mini:latest
phi4_mini_latest <- "phi4-mini:latest"


score_local_llm <- function(text_line, model = "llama3.2:latest") {
  # Define the core logic within a tryCatch to handle failures internally
  result <- tryCatch({
    promp_text <- paste("You are a sentiment analysis API.
                        Return ONLY a JSON object with keys 'emotion_label' and 'confidence_score'.
                        Allowed labels: Joy, Optimism, Anger, Sadness, Neutral. Input text:", text_line)
    
    res <- generate(
      model = model,
      prompt = promp_text,
      output = "text",
      temperature = 0
    )
    
    # Extract JSON string
    json_start <- regexpr("\\{", res)
    json_end <- regexpr("\\}", res)
    json_str <- substr(res, json_start, json_end)
    
    # Parse
    parsed <- fromJSON(json_str)
    
    
    # Validate the label
    if (!parsed$emotion_label %in% c("Joy", "Optimism", "Anger", "Sadness", "Neutral")) {
      parsed$emotion_label <- "Neutral" 
    }
    
    return(parsed)
    
  }, error = function(e) {
    # If the model fails or returns non-JSON, return this default list
    return(list(emotion_label = "Neutral", confidence_score = 0.0))
  })
}


# Claude API Connection

Sys.getenv("CLAUDE_API_KEY")

### Get the list of Claude Models available
get_clean_model_list <- function() {
  req <- request("https://api.anthropic.com/v1/models") |> 
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01"
    )
  
  resp <- req_perform(req)
  data <- resp_body_json(resp)$data
  
  # Flatten and convert to a clean tibble
  model_df <- map_dfr(data, ~tibble(
    id = .x$id,
    display_name = .x$display_name,
    created_at = .x$created_at
  ))
  
  return(model_df)
}

get_clean_model_list()


## Build the main scoring function
score_claude_llm <- possibly(function(text_input) {
  
  model_id <- "claude-sonnet-5"
  
  # Construct the API request
  req <- request("https://api.anthropic.com/v1/messages") |> 
    req_headers(
      "x-api-key" = Sys.getenv("CLAUDE_API_KEY"),
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    ) |> 
    req_body_json(list(
      model = model_id,
      max_tokens = 100,
      system =
        'You are a professional sentiment analysis engine. Your task is to analyze the emotional tone of the provided text and output a single JSON object.
            Follow these strict constraints:
              1. Output Format: Return ONLY valid JSON. Do not include markdown code blocks, backticks, or any explanatory preamble.
              2. Keys Required: 
                  - "emotion_label": Must be exactly one of: "Joy", "Optimism", "Anger", "Sadness", or "Neutral".
                  - "confidence_score": A numeric value between 0.0 and 1.0.
              3. Behavior: 
                  - Do not provide conversational responses, justifications, or meta-commentary.
                  - If the input is ambiguous, select the most probable label and assign a lower confidence score.
                  - If the input is empty or nonsensical, output {"emotion_label": "Neutral", "confidence_score": 0.0}.',
                  messages = list(list(role = "user", content = text_input))
      ))
  
  # Execute the request
  resp <- req_perform(req)
  # Parse the response
  resp_body <- resp_body_json(resp)
  
  content_text <- resp_body$content[[1]]$text
  
  # Remove potential markdown code blocks
  clean_json <- gsub("(?s).*(\\{.*\\}).*", "\\1", content_text, perl = TRUE)
  clean_json <- trimws(clean_json)
  
  # Extract JSON from the string
  result <- fromJSON(clean_json)
  
  return(result)
  
}, otherwise = list(emotion_label = NA, confidence_score = NA))


## Create a wrapper function that builds a data frame from responses
analyze_lyrics_claude <- function(lyrics_df) {
  lyrics_df |>
    mutate(sentiment_data = map(text, ~score_claude_llm(.x))) |> 
    unnest_wider(sentiment_data)
}








