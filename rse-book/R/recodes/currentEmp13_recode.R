currentEmp13_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # 1) Digital humanities and LIS
  "(?i)digital\\s*humanit|\\bdh\\b|digital humanitit|library|information\\s+science\\b|informationswissenschaft|bibliotec|bilbiotecas y apoyo a la investigación", 
  "Digital humanities and LIS",

  # 2) Earth and environmental sciences (incl. geo-, meteo-, climate, ecology)
  "(?i)environment|ecolog|earth|\\beath\\b|geolog|geochem|geoscien|geowissenschaft|geographie|\\bgeography\\b|geoinformat|meteorolog|climate\\s*science",
  "Earth and environmental sciences",

  # 3) Physics and space sciences (astro, cosmology, particle, planetary, nuclear physics)
  "(?i)astro|cosmo|particle\\s*physics|solar\\s*system|planetary|nuclear\\s*physics|space\\b",
  "Physics and space sciences",

  # 4) Chemistry and materials
  "(?i)\\bchem|chemical\\s*sciences|electro\\s*chem|material\\s*science",
  "Chemistry and materials",

  # 5) Life sciences and neuroscience
  "(?i)bioinform|neurosci|neurowissenschaft",
  "Life sciences and neuroscience",

  # 6) Computing, data and engineering
  "(?i)aerospace|robotics|high\\s*performance\\s*computing|\\bhpc\\b|software\\s*development|infrastructure|security\\b|fintech|information\\s*systems|data\\s*visuali|\\brse\\b|research\\s*data\\s*management|\\bstatistics\\b",
  "Computing, data and engineering",

  # 7) Humanities and social sciences
  "(?i)\\bhumanities\\b|geisteswissenschaft|kunstwissenschaft|literature|geschichts|justice\\b|sciences\\s+de\\s+l'?orientation|social\\s*sciences",
  "Humanities and social sciences",

  # 8) Cross-cutting / research-wide roles
  "(?i)any\\s*really|so\\s*all\\s*of\\s*the\\s*above|research\\s*agnostic|operate\\s*university|depends\\s*on\\s*who\\s*asks|general\\s*consulting|anwender\\s*sind\\s*aus\\s*allen\\s*möglichen\\s*gebieten|natural\\s*sciences\\b|we\\s*operate\\s*university",
  "Cross-cutting / research-wide",
  
  "archaeology", "Archaeology"
)
