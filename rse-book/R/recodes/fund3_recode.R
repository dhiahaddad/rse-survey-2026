fund3_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # Public grants and programmes
  "(?i)\\b(european project|eu(rope)?)(s)?\\b", "Public grants and programmes",
  "(?i)\\b(federal funding|national funding)\\b", "Public grants and programmes",

  # Private/philanthropy
  "(?i)\\b(private philanthropy|philanthropy)\\b", "Private/philanthropic support",

  # Earned/contract income (includes subscriptions + service contracts + freelance)
  "(?i)\\b(service contracts?)\\b", "Earned/contract income",
  "(?i)\\b(subscriptions?)\\b", "Earned/contract income",
  "(?i)\\b(self[- ]employment|self employment|freelance|consult(ing|ancy))\\b", "Earned/contract income",

  # Institutional/core
  "(?i)\\b(institutional|core funding).*\\b|\\bsupposed to become institutional over the next years\\b",
  "Institutional/core funding"
)
