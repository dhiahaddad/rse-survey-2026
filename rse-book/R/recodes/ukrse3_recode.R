ukrse3_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # AI-assisted learning
  "(?i)with ai help|prompt for code", "AI-assisted learning",

  # Academic research experience (PhD/postdoc)
  "(?i)during my (own )?phd|postdoc[s]?|while working on my phd", "Academic research (PhD/Postdoc)",

  # Apprenticeships and industrial placements
  "(?i)apprenticeship|industrial placement", "Apprenticeship/industrial placement",

  # Industry and prior employment experience
  "(?i)industry\\b|private sector|previous (employment|industry experience)|learned on[- ]the[- ]job.*industry|previous work as consultant", 
  "Industry/prior employment experience",

  # Employer-provided or vocational training programs
  "(?i)courses? (offered|organised) by (my )?employer|attending training courses while previously employed|qualifikationsma(ß|ss)nahme.*bundesagentur",
  "Employer-provided/vocational training",

  # Institutional/national HPC centres and RC staff training
  "(?i)courses offered by csc in finland|university high[- ]performance computing center staff",
  "Institutional/national HPC/RC training",

  # Formal education (degrees, school)
  "(?i)studium|habe informatik studiert|other school",
  "Formal education (degrees/school)",

  # Self-directed online resources
  "(?i)you ?tube|online articles|free talks|books|stack overflow", 
  "Self-directed online learning",

  # Peer learning and mentorship
  "(?i)peers|co[- ]?working with a software engineer", 
  "Peer learning/mentorship",

  # Open source contributions
  "(?i)working on open source projects", 
  "Open source contributions",

  # Clarifications/other
  "(?i)i'm answering here|rather than software development generally|not in my current role in academia", 
  "Other/clarification"
)
