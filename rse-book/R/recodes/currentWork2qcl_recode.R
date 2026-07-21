currentWork2qcl_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # External institutional partners (national or cross-institution)
  "(?i)instituci(ó|o)nes|\\binstitution(s)?\\b", "External institutional partners",

  # Project-based collaborators (incl. international projects)
  "(?i)project[- ]based", "Project-based collaborators",

  # Within institution (department or university)
  "(?i)within the university|\\bdepartment\\b", "Within institution (dept/university)",

  # Field-specific researchers
  "(?i)\\bfield of research\\b|\\bmy field\\b|researchers in our field", "Field-specific researchers",

  # Business/internal automation use
  "(?i)business automation", "Business process automation",

  # Global or international researchers
  "(?i)global(ly)?\\)?|worldwide|international(ly)?|anywhere|no matter which country|forschende international|researchers around the world|researchers worldwide|global research area",
  "Global/international researchers",
  "(?i)in principle anyone|anyone who wants to use|regardless of affiliation|international projects|maintained public services", "Global/international researchers"
)
