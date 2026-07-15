currentEmp60_0_recode_map <- tibble::tribble(
  ~raw, ~clean,

  "(?i)research software engineer|\\brse\\b|research software developer",
  "Research Software Engineer / Developer",
  "(?i)senior.*(research software|rse|software engineer)|lead.*(research software|rse)",
  "Senior / lead research software role",
  "(?i)postdoc|post-doctoral|phd student|doctoral candidate",
  "PhD student / postdoctoral researcher",
  "(?i)professor|associate professor|assistant professor|lecturer|faculty",
  "Faculty / professor",
  "(?i)research fellow|research associate|researcher|forsker",
  "Researcher (other title)",
  "(?i)software engineer|software developer|developer|programmer|ingeniør|engineer",
  "Software engineer / developer",
  "(?i)data scientist|data engineer|bioinformatician|computational",
  "Data / computational specialist",
  "(?i)manager|head of|director|team lead|group lead",
  "Management / team lead",
  "(?i)technician|specialist|consultant|analyst|coordinator|administrator",
  "Technical / professional support role"
)
