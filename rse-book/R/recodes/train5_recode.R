train5_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # Course materials/infrastructure development (apps, packages, setup, tooling)
  "(?i)develop(ped|ment).*web (applications|apps|packages)|documentation and setup|software and tools.*lecture|containers",
  "Course materials/infrastructure development",

  # Lecturer / instructor (incl. guest lecturer, teaching course sections)
  "(?i)guest lecturer|teaching .*course section|lecture\\b",
  "Lecturer / instructor",

  # Tutor roles
  "(?i)\\btutor\\b",
  "Tutor",

  # Practical sessions / labs
  "(?i)practical sessions?",
  "Practical/lab instructor",

  # Supervision / mentoring
  "(?i)supervision",
  "Supervision / mentoring",

  # Ad hoc cover
  "(?i)ad hoc replacement",
  "Ad hoc teaching cover"
)
