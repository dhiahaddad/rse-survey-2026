train3_0_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # 2) Carpentries and CodeRefinery
  "(?i)carpentr(y|ies)|software\\s*carpent(y|ry)",
  "The Carpentries",
  "(?i)code\\s*refinery|coderefinery",
  "CodeRefinery",

  # 3) University teaching and academic courses (lectures, curriculum, MSc/BSc/PhD)
  "(?i)university (courses?|lectures?|training|provided)|standard university curriculum|postgraduate courses|msc( applied mathematics)?|bsc( applied mathematics)?|space science programme|african institute .*masters|courses? at (u[it]|university)|guest lecture|teaching (at universities|in the regular curriculum|undergraduates)|tutorial leader in physics|class(room)?|lectures?\\)?|my university|universidad|universit(ä|a)tskurse|guest lecturing at university|open to ms|phd levels for applied researchers",
  "University teaching and academic courses",

  # 4) Local/in-house/internal trainings
  "(?i)in[- ]?house|local (ai|chapter|computer science|graduate school|institute|institution|organization|research group|scripting|staff|training|uni|university|user)|zentrumsinterne schulungen|lokale (universit(ä|a)tskurse|weiterbildungen)|provided through our organisation|offered through our organisation|our own training|we run our own training|team training|onboarding|reading group",
  "Local/in‑house trainings",
  "local|interne frameworkschulungen|private trainings|i am training phd trainees in my research laboratory|post docs. i am responsible for the material", "Local/in‑house trainings",

  # 5) Workshops, tutorials, hackathons, user trainings (generic and at conferences)
  "(?i)workshop(s)?|tutorials?( at| on)?( workshops| scientific meetings| python| agu| gem)?|user meetings|bring your own code|tutorial hackathon|hackathon(s)?|gpu hackathons|any dedicated user training workshop|dedicated totorials to interest groups",
  "Workshops, tutorials, and hackathons",

  # 6) HPC, parallel programming, and research computing trainings/providers
  "(?i)\\bhpc\\b|parallel programming|running climate models|olcf|epcc training|lumi trainings|nvidia (dli|introduction to deep learning)|pyhc workshop|reannz training|afretec workshops|chpc\\b|eosc projects training|various nsf ncar efforts|tsmp fall school|grid|intersect\\b",
  "HPC and research computing trainings",

  # 7) Version control, Git/GitHub/GitLab, and RSE basics
  "(?i)\\bgit(?!hub)\\b|git (training|migration|introduction)|gitlab( introduction|/github ci/cd pipelines)?|github( actions)?|vcs\\b|versionskontrolle|rse basics.*version control",
  "Version control and RSE basics",

  # 8) Python and general programming courses
  "(?i)python|introductot?ion to python|advanced python|advance python|teaching a course on beginner'?s programming|programming courses? for researchers|basic programming|computer science|course in computing for scientists|recode.*",
  "Python and general programming",

  # 9) AI/ML and data science trainings
  "(?i)\\bai\\b|ai factories training|ml( software)?\\b|jax behavior workshop|data (literacy|fairness|working with data)|digital literacy",
  "AI/ML and data science trainings",

  # 10) Research software engineering practices, tools, and workflows
  "(?i)software development (best practices)?|rse training|introduction to scientific software|workflow development|conda\\b|cookiecutter|documentation (tools|management|writing)|developer training|project training|deployment\\b|domain training|programming courses for researchers|google training|project pythia|escience center digital skills program|i often attend courses out on by oxford rse|tests",
  "Research software engineering practices and tools",

  # 11) Community and external initiatives (societies, programs, networks)
  "(?i)google summer of code|rladies|rse chile|galaxy training network|genomics aotearoa training|training organised by africa cdc|african institute for mathematical sciences|european materials modelling council|elwazi trainings|ncrm|zbiw|brseqtb training|project pythia|bespoke client training programms|i sometimes talk at developer meetups|other public health laboratories|research council funded training|stem innovations|training for the funded consortia that i am part of",
  "Community and external initiatives",

  # 12) Online/self-directed learning
  "(?i)online|coursera|random youtube videos|autodidaktisch|distanzkurse der universität|self (developped|made)|developing$|formación en línea",
  "Online and self‑directed learning",
  "see https://interactivedatascience.courses/", "Online and self‑directed learning",

  # 13) Institution/organisation-specific or national facilities
  "(?i)tu dresden|escience center|hifis education ?/ ?hida|hifis schulugen|training organised by my organisation|training at universities in federal state|offered through our organisation|local institution(s)?|local institute training|erasmus|formación company|leadership training",
  "Institution/organisation‑specific programs",

  # 14) Domain‑specific or tool‑specific trainings
  "(?i)eln introduction|university courses on dft|genome assembly|iot workshops|mechanical engineering design|latex|linux|netdrive|conda|vcs|and gis|emerging technologies in the health field|farm machinery\\)|i run training specific to our research community on tools|ci bioinformatics training|specialized for our tool|suresoft|training in our software|training on research equipment",
  "Domain- or tool-specific trainings",
  
  ".* schools?|bsa reu|colegio de verano interno", "Summer/Fall schools",
  
  "working with data|formación de data steward", "Data Management",
  
    # 1) No/none/unspecified programs
  "(?i)^(n\\.?a\\.|none|zero|keine|ninguno)$|no\\s+formal|nothing\\s+formal|although it'?s not extremely structured|once$|similar$", NA_character_,
  "education program|oareidiz", NA_character_,
  "offering internships", NA_character_,
  "practicalmeeg", NA_character_,
  "(?i)^courses$", NA_character_,
  "bespoke training", NA_character_,
  "bespoke for field only", NA_character_,
  "small training groups", NA_character_,
  "ci\\)", NA_character_
)
