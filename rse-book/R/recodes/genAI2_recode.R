genAI2_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # Evaluation / decision support (pros/cons, second opinion)
  "(?i)pros and cons|trade[- ]?off|second opinion|abw(ä|a)gen|choices", 
  "Evaluation and decision support",

  # Prototyping and scaffolding (PoC, templates, outlines, Dockerfiles, parsers)
  "(?i)\\bpo[cf]\\b|proof of concept|scaffold|template(s)?\\b|code outline|outline(s)?\\b|dockerfile|pattern implementation|prototyp|writing parsers",
  "Prototyping and scaffolding",

  # Code comprehension and problem solving (understand/navigate code, rubber ducking)
  "(?i)understanding( ill-documented)? code|make? ?ing sense of code|navigate.*code ?bases?|responsive rubber duck(y)?|rubber duck|problem solving|verstehen von code|prompting for a solution of a task that does not seem to have a straightforward answer on forums",
  "Code comprehension and problem solving",

  # Code assistance and refactoring (syntax, APIs, libraries, formatting, naming, tooling)
  "(?i)checking syntax|get(ting)? (information )?about syntax|get(ting)? help with (libraries|library|api)|which libraries exist|question on how to use a library|refactor(ing)?|formatting|naming variables|tooling|testing crazy ideas.*",
  "Code assistance and refactoring",

  # Code review and linting
  "(?i)code review(s)?|review(ing)? (my )?code|linting",
  "Code review and linting",

  # Documentation (docs discovery)
  "(?i)documentations|documenting existing codebases|entry points? to documentation|learn(ing)? (new|unknown)? (software )?technolog|finding resources|domain knowledge",
  "Documentation",
  "understanding ill-documented legacy code", "Documentation",

  # Brainstorming and planning
  "(?i)brainstorm|conceptualis(e|ing)|planning|getting ideas|exploratory prigramming",
  "Brainstorming and planning",

  # Writing and communication (editing, summarizing, emails, blogs, presentations)
  "(?i)improv(ing|e) (the )?wording|spelling/grammar|grammar checking|summari(s|z)e|check my own writing|writing (documents|non-technical overviews)|emails?|blog posts|turning my .*infodumps|general non-technical communication|making images? for presentations",
  "Writing (grammar/spelling) and communication",

  # Data and analysis tasks (extraction, NLP, enrichment, plotting, sentiment)
  "(?i)data extraction|datenanreicherung|natural language processing|text classification|sentiment analysis|plotting of data",
  "Data and analysis tasks",
  
  # Writing
  "papers", "Papers",
  
  # Learning
  "learn how to apply new or unknown technologies|ask(ing)? questions of rag|\\brag[- ]?based|research on technical solutions|to get the domain knowledge that i need for the current projects|double check techniques.*|where i can learn the details error free|best practices", "Learning",
  
  # NAs
  "specially when it's a technology i'm not very familiar with.|etc.", NA_character_,
  "(?i)ai is rarely good enough|mostly checking if llms can be helpful|^etc\\.?$|have a laugh|launching pad", NA_character_
)
