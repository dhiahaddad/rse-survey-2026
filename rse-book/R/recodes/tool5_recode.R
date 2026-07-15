tool5_recode_map <- tibble::tribble(
  ~raw, ~clean,

  # Specific services and platforms
  "(?i)\\bbinderhub\\b", "BinderHub",
  "(?i)^cran$", "CRAN (R package repository)",
  "(?i)github\\s*actions", "CI/CD platform (GitHub Actions)",

  # Kubernetes / OpenShift (incl. mentions of Docker/Kubernetes platforms and pod capacity)
  "(?i)\\bk8(s)?\\b|kubernetes|openshift|docker/kubernetes|spin up kubernetes pods",
  "Kubernetes/OpenShift",

  # University-hosted infrastructure
  "(?i)university hosted (docker/kubernetes platforms|virtual servers)",
  "University-hosted infrastructure",

  # Embedded / IoT / Edge / Avionics / Industrial PCs
  "(?i)avionics|embedded( system)?\\b|industrial pc[s]?|iot( edge)?",
  "Embedded/IoT/Edge",

  # Virtual machines / local VMs
  "(?i)local\\s*(\"?cloud\"?)?\\s*\\(=\\s*vms?\\)?|local virtual machines?|virtual machine.*zentraler serverinfrastruktur|\\bvms?\\b",
  "Virtual machines (local/on-prem)",

  # Private/on-prem cloud
  "(?i)private\\s*cloud|we have our own cloud infrastructure",
  "Private/on‑prem cloud",

  # Clusters / Grid / Server farms
  "(?i)\\bgrid\\b|self[- ]maintained cluster|server farms|cluster\\b",
  "Clusters/Grid (on‑prem)",

  # Local/individual machines
  "(?i)^local$", "Local (on‑prem)",
  "(?i)workstations?", "Workstations"
)
