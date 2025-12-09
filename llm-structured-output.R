library(ellmer)
library(tidyverse)

chat <- chat_ollama(model = "llama3.1:8b")

# Basics

chat$chat_structured(
  "My name is Susan and I'm 13 years old",
  type = type_object(
    age = type_number(),
    name = type_string()
  )
)

# Article summarisation

text <- readLines(system.file(
  "examples/third-party-testing.txt",
  package = "ellmer"
))

type_summary <- type_object(
  "Summary of the article.",
  author = type_string("Name of the article author"),
  topics = type_array(
    type_string(),
    'Array of topics, e.g. ["tech", "politics"]. Should be as specific as possible, and can overlap.'
  ),
  summary = type_string("Summary of the article. One or two paragraphs max"),
  coherence = type_integer(
    "Coherence of the article's key points, 0-10 (inclusive)"
  ),
  persuasion = type_number("Article's persuasion score, 0.0-1.0 (inclusive)")
)

data <- chat$chat_structured(text, type = type_summary)

cat(data$summary)

str(data)


# Named entity recognition

text <- "
  John works at Google in New York. He met with Sarah, the CEO of
  Acme Inc., last week in San Francisco.
"

type_named_entity <- type_object(
  name = type_string("The extracted entity name."),
  type = type_enum(c("person", "location", "organization"), "The entity type"),
  context = type_string("The context in which the entity appears in the text.")
)

type_named_entities <- type_array(type_named_entity)

chat$chat_structured(text, type = type_named_entities)


# Text classification

text <- "The new quantum computing breakthrough could revolutionize the tech industry."

type_score <- type_object(
  name = type_enum(
    c(
      "Politics",
      "Sports",
      "Technology",
      "Entertainment",
      "Business",
      "Other"
    ),
    "The category name",
  ),
  score = type_number(
    "The classification score for the category, ranging from 0.0 to 1.0."
  )
)

type_classification <- type_array(
  type_score,
  description = "Array of classification results. The scores should sum to 1."
)

data <- chat$chat_structured(text, type = type_classification)

print(data)


# Example

chat <- chat_ollama(
  model = "gemma3:4b",
  system_prompt = "you are a medical expert and specialist in oncology. Your task is to carefully analyze the clinical texts and classify whether they are related or not to any oncological pathology.
                    I don't want any explanation. Just give me the answer in the requested format."
)

text <- c("mastectomia radical tumorectomia con vaciamiento ganglionar total")

chat$chat_structured(
  text,
  type = type_object(
    cancer = type_enum(
      "Clasify if related to cancer",
      values = c("Related", "Not related")
    )
  )
)

# Aplicar la clasificación a cada elemento del vector y devolver un data frame

text <- c(
  "mastectomia radical tumorectomia con vaciamiento ganglionar total",
  "Hipertensión arterial descompensada",
  "Cáncer de pulmón en etapa avanzada",
  "Miastenia gravis de larga data"
)

result <- map_df(
  text,
  ~ tibble(
    text = .x,
    cancer = chat$chat_structured(
      .x,
      type = type_object(
        cancer = type_enum(
          "Clasify if related to cancer",
          values = c("Related", "Not related")
        )
      )
    )
  )
) |>
  mutate(cancer = unlist(cancer))

print(result)
