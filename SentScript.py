from datasets import Dataset
from transformers import pipeline
import numpy as np
import pandas as pd

debates_df = pd.read_csv('ParlyDebatesLabeled2.csv')
#debates_df.label[debates_df.label == "Norma"] = "Normative"
debates_df.label[debates_df.label == "Delay"] = "Delay_Denial"
debates_set = Dataset.from_pandas(debates_df.reset_index(drop=True))

categorise = pipeline("sentiment-analysis")

def split_text_into_chunks(text, max_chunk_length=510):
    # Remove newlines and other whitespace characters
    cleaned_text = " ".join(text.split())
    chunks = []
    start = 0
    while start < len(text):
        end = start + max_chunk_length
        chunk = text[start:end]
        chunks.append(chunk)
        start = end
    return chunks

# Process each chunk and accumulate label scores
def get_best_score():
  all = []
  for i in range(0,len(debates_set)):
    chunks = split_text_into_chunks(debates_set["text"][i], max_chunk_length=510)
    label_scores = {}
    for i in range(len(chunks)):
      results = categorise(chunks[i])
      for result in results:
        label = result['label']
        score = result['score']
        if label in label_scores:
            label_scores[label].append(score)
        else:
            label_scores[label] = [score]

    # Calculate the average score for each label
    average_scores = {label: sum(scores) / len(scores) for label, scores in label_scores.items()}

    # Find the label with the highest average score
    best_label = max(average_scores, key=average_scores.get)
    all.append([best_label,average_scores[best_label]])

    #return [best_label,{average_scores[best_label]}]
  return all

responses = get_best_score()

debates_sent = pd.DataFrame(responses, columns = ["sentiment", "sentscore"])
new_debs=pd.concat([debates_df, debates_sent], axis = 1)

conditions=[
    ((new_debs.label == "Delay_Denial") & (new_debs.sentiment == "POSITIVE")),
    ((new_debs.label == "Delay_Denial") & (new_debs.sentiment == "NEGATIVE")),
    ((new_debs.label == "Normative") & (new_debs.sentiment == "POSITIVE")),
    ((new_debs.label == "Normative") & (new_debs.sentiment == "NEGATIVE")),
    (new_debs.label == "Economic justification")
]

values = ["Delay_Denial", "Normative" ,"Normative" , "Delay_Denial", "Economic justification"]
new_debs['new_label'] = np.select(conditions, values)


new_debs.to_csv("RelabeledDebates.csv")