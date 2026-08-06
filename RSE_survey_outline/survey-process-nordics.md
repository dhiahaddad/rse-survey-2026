# International RSE Survey 2026 — Nordic Survey Process

Questions **shown to Nordic respondents** with at least one answer (**N > 0**). Includes routing and filtering rules where they apply.

Countries in scope match `FILTER` in `rse-book/R/.config`:

```r
FILTER = c("Finland", "Norway", "Sweden", "Denmark", "Iceland", "Estonia")
```

Derived from `RSE_survey_2026_data/2026_tf.csv` and `2026_cols.csv`.

---

## Analysis filtering (all Nordic questions)

Before question-level **N** is computed, every Nordic analysis applies:

| Step | Field | Rule | Result |
|------|-------|------|--------|
| 1. Submission | `submitdate_0` | Non-empty | **62** submitted |
| | | Partial sessions excluded | **10** excluded |
| 2. Country | `socio1_0` | In `FILTER` | **62** Nordic respondents |
| 3. Question | per question | At least one non-empty answer | **N** in tables below |

### Submitted Nordic sample by country

| Country | n |
|---------|---|
| Finland | 20 |
| Sweden | 19 |
| Norway | 10 |
| Estonia | 8 |
| Denmark | 4 |
| Iceland | 1 |
| **Total** | **62** |

**N** counts submitted Nordic respondents who answered that question. Multi-select: any option selected; Likert: any sub-item answered; single Yes/No: includes False.

---

## Survey routing for Nordic respondents

At **`socio1`** (country), Nordic respondents enter the Nordic path and **do not see** Germany/UK/US/Netherlands/Switzerland/South Africa-specific variants (e.g. `currentEmp1qde`, `org3de`, `ref1uk`, `turnOver3zaf`).

Conditional follow-ups observed in Nordic data:

| Trigger | Follow-up | Eligible (N) | Answered (N) |
|---------|-----------|--------------|--------------|
| `socio1_0` ∈ Nordics | `org3nord`, `org4nord`, `currentWork3nord`, `fund1nord`, `skillNord` | 62 | 58–60 |
| `conf1can = Yes` | `conf2can` | 27 | 24 |
| `org1can = Yes` or RSE association member | `org2can` | — | 47 |
| `currentEmp6 = Yes` | `currentEmp60` | 17 | 16 |
| `genAI1 ≠ Never` | `genAI2`, `genAI3` | 51 | 50 / 49 |
| `train4 = Yes` | `train5` | 29 | 29 |

This document lists **72** question groups shown to Nordics with N > 0 (of 110 total in the survey).

---

## Questions shown to Nordics (N > 0)

### Survey metadata

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 1 | `startlanguage` | Single / free-text | 62 | — | Start language |
| 3 | `startdate` | Single / free-text | 62 | — | Date started |

### Other

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 2 | `seed` | Single / free-text | 62 | — | Seed |

### Screening

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 4 | `socio1` | Single / free-text | 62 | Routes respondents to Nordic-specific blocks (`org3nord`, `org4nord`, `currentWork2qcl`, `currentWork3nord`, `fund1nord`, `skillNord`) and away from other regional variants. | In which country do you work? |

### RSE role

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 5 | `rse1` | Single / free-text | 62 | — | Do you write software for academic research as part of your job? |
| 7 | `rse3` | Single / free-text | 58 | — | Who uses the code that you write? |
| 11 | `rse4de` | Single / free-text | 61 | — | Does the majority of your role comprise leading a group of software developers or RSEs? |

### Education

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 6 | `edu2` | Single / free-text | 62 | — | In which discipline is your highest academic qualification? |
| 110 | `edu1` | Single / free-text | 62 | — | What is the highest level of education you have attained? |

### Software experience

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 8 | `soft2can` | Single / free-text | 61 | — | Do you consider yourself a professional software developer? |
| 9 | `soft1can` | Single / free-text | 62 | — | How many years of software development experience do you have? |

### Open science

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 10 | `open1de` | Single / free-text | 62 | — | Do you have an ORCID ID? |
| 106 | `open1can` | Single / free-text | 59 | — | How often do you license your software with an open-source licence? |

### UK RSE network

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 12 | `ukrse1` | Single / free-text | 59 | — | Are you a member of any of the following associations of Research Software Engineers (called "Research Software Developers" in Canada)? |
| 95 | `ukrse3` | Multi-select | 61 | — | How did you learn the skills you need to become a Research Software Engineer / Research Software Developer? |

### RSE organisation

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 13 | `org1can` | Single / free-text | 28 | Global question; `org2can` follows if respondent is interested or an association member. | Would you be interested in joining such an organisation? |
| 16 | `org2can` | Multi-select | 47 | Follow-up when `org1can = Yes` or respondent is an RSE association member (`ukrse1`). | What would you hope to get out of such an organisation (check all that apply)? |
| 21 | `org3nord` | Multi-select | 60 | Shown when `socio1_0` is a Nordic country. | How could Nordic-RSE or a potential Nordic research software institute foster community engagement and knowledge transfer? |
| 22 | `org4nord` | Multi-select | 60 | Shown when `socio1_0` is a Nordic country. | With Nordic-RSE we have an association dedicated to the community of Research Software Engineers in the Nordics. What tasks do you think should be undertaken by Nordic-RSE or a future Nordic institute for research software? |

### Employment

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 23 | `currentEmp1` | Multi-select | 62 | Nordics see `currentEmp1` (not `currentEmp1qde`, `currentEmp1nl`, etc.). | Please select your organization type |
| 36 | `prevEmp1` | Multi-select | 62 | Nordics see `prevEmp1` (not `prevEmp1qde`, `prevEmp1nl`, etc.). | Where was your previous job based? |
| 40 | `currentEmp5` | Single / free-text | 56 | — | What is your official job title? |
| 41 | `currentEmp6` | Single / free-text | 55 | If Yes, respondent is asked `currentEmp60` for alternate job title. | Are you known in your group by a different job title? |
| 42 | `currentEmp60` | Single / free-text | 16 | Follow-up when `currentEmp6 = Yes`. | Please enter the job title you use. |
| 43 | `currentEmp12` | Single / free-text | 62 | — | Do you work full time or part time? |
| 44 | `currentEmp10` | Multi-select | 62 | — | What is the nature of your current employment? |
| 45 | `currentEmp11` | Single / free-text | 13 | — | What is the expected duration (in years) of your current position (in total)? |
| 46 | `currentEmp13` | Multi-select | 62 | — | Please select the discipline(s) in which you work. Please select all that apply. |
| 109 | `currentEmp2q` | Single / free-text | 38 | — | Which university do you work for? |

### Attitudes (Likert)

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 47 | `likert0` | Likert | 62 | — | On average, how much of your time is spent on |
| 48 | `likert1` | Likert | 62 | — | On average, how much time would you like to spend on |
| 49 | `likert3a` | Likert | 62 | — | In general, I am satisfied with my current position. |
| 50 | `likert3b` | Likert | 61 | — | In general, I am satisfied with my career. |
| 52 | `likert4b` | Likert | 60 | — | I feel that my contribution to research is recognised by my institution. |
| 54 | `likert5b` | Likert | 60 | — | NA |
| 55 | `likert5a` | Likert | 60 | — | NA |
| 105 | `likert2a` | Likert | 53 | — | When you are writing about your software, do you reference the software directly or do you reference a paper describing your software? |
| 107 | `likert2b` | Likert | 53 | — | How often do you generate a DOI or other persistent identifier for your software? |

### Turnover

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 56 | `turnOver3` | Single / free-text | 61 | — | How likely are you to choose to leave your job in the next 12 months? |

### Work activities

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 64 | `currentWork2` | Single / free-text | 59 | — | Are you part of a dedicated research software group within your institution? |
| 65 | `currentWork1` | Single / free-text | 62 | — | Do you always work with the same researchers, or do you regularly change the researchers you work with? |
| 66 | `currentWork2qcl` | Multi-select | 62 | Shown to Nordics (and Chile via QCL path). Replaces generic audience question for this group. | Who are you writing code for, supporting with your code or providing services to? |
| 71 | `currentWork3nord` | Single / free-text | 58 | Shown when `socio1_0` is a Nordic country. | Do you feel like you have a network of peers that extends beyond your close colleagues? (By "peers", we mean other people in a similar position to yours; please do not count networks that you may have built before, e.g. during a PhD) |

### Publications

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 67 | `paper3mod` | Single / free-text | 62 | — | Have you published a paper about your software? (For example in journals such as JOSS or Software-x) |
| 108 | `paper2mod` | Multi-select | 50 | — | In general, when your software contributes to a paper, are you acknowledged in that paper? |

### Conferences

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 68 | `conf1can` | Single / free-text | 57 | Global question; `conf2can` follows when answer is Yes. | Have you presented your software work at a conference or workshop? |
| 69 | `conf2can` | Single / free-text | 24 | Follow-up when `conf1can = Yes`. | At which conference(s)/workshop(s) have you presented your software work? |

### Project management

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 72 | `proj1can` | Single / free-text | 61 | — | How many software projects are you currently involved in? |
| 75 | `proj8can` | Single / free-text | 58 | — | Do your research software projects typically include a project manager? |
| 77 | `proj4can` | Multi-select | 61 | — | How do you test the software that you produce? Please check all that apply. |
| 78 | `proj5zaf` | Multi-select | 61 | — | Which version control tools do you use for software development? |
| 79 | `proj6zaf` | Multi-select | 61 | — | Which collaboration tools do you use for software development? |

### Job stability

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 73 | `stability1` | Single / free-text | 56 | — | What is the minimum number of software developers that would have to suddenly disappear from your current project before it stalls due to a lack of knowledgeable people? |
| 74 | `stability2` | Single / free-text | 59 | — | Do the projects you work on typically have a plan to cope with developers leaving the group? |

### Training & skills

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 81 | `train2` | Single / free-text | 61 | — | On average, how many times a year do you take part in providing training? |
| 83 | `train3` | Single / free-text | 32 | — | What training programs are you involved with? |
| 84 | `train4` | Single / free-text | 57 | If Yes, respondent is asked `train5` about teaching contribution. | Do you teach or support the teaching of credit bearing modules or courses? |
| 85 | `train5` | Multi-select | 29 | Follow-up when `train4 = Yes`. | In what capacity do you contribute? |
| 96 | `skillNord` | Single / free-text | 60 | Shown when `socio1_0` is a Nordic country. | Are you granted time for personal skills acquisition or professional development? (For example, learning a new technique that is not specifically needed for a current project) |
| 97 | `skill2` | Single / free-text | 40 | — | What three skills would you like to acquire or improve to help your work as a Research Software Engineer / Research Software Developer? The skills can be technical and non-technical. |

### Funding

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 86 | `fund3` | Multi-select | 59 | — | Which of the following sources are used to pay for your effort as a Research Software Engineer? |
| 90 | `fund1nord` | Single / free-text | 60 | Shown when `socio1_0` is a Nordic country. | Do you have access to grants in your institutions/country for RSE type of work (ie not research grants)? |

### Tooling

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 91 | `tool5` | Multi-select | 58 | — | On which platform(s) is your most recent research software project deployed? Please select all that apply. |
| 93 | `tool2` | Multi-select | 60 | — | Which operating system do you primarily use for development? |
| 94 | `tool4can` | Multi-select | 61 | — | What programming languages do you use at work? Please select all that apply. |

### Generative AI

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 98 | `genAI1` | Single / free-text | 61 | If answer is not Never, `genAI2` and `genAI3` are shown. | How frequently do you use generative AI tools in your RSE-related tasks (e.g., coding, documentation, testing, or design)? |
| 99 | `genAI2` | Multi-select | 50 | Follow-up when `genAI1 ≠ Never`. | For what purposes do you use generative AI tools? |
| 100 | `genAI3` | Multi-select | 49 | Follow-up when `genAI1 ≠ Never`. | Which generative AI tools do you currently use in your RSE work? |
| 101 | `genAI4` | Single / free-text | 54 | — | Over the next five years, how do you think generative AI tools will affect demand for RSEs roles? |
| 102 | `genAI5` | Multi-select | 59 | — | Over the next five years, how do you think generative AI tools will affect |
| 103 | `genAI6` | Single / free-text | 43 | — | What skills do you think are necessary to effectively use generative AI for research software development tasks? |

### Demographics

| # | Code | Type | N | Filtering / routing | Question |
|---|------|------|---|---------------------|----------|
| 104 | `socio3` | Single / free-text | 61 | — | Please select your age |

---

*See also [`survey-process.md`](survey-process.md) for the full global survey flow.*

