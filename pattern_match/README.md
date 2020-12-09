Pattern Match
================
Robby
5/4/2020

This script is created to automatically look up and match species and
observer name from external source whether from field survey and
observation or other biodiversity resources with existing local database
for fauna and flora that has been recorded in PT Austindo Nusantara Jaya
Tbk conservation areas in order to know which data entry is new or
already recorded (Exist in local database). For more precise
calculation, the script also corrects typos in names before pairing them
with local database, the function `stringsim` from *stringdist* package
will be used as the main function for the pattern matching and
correction.

The process is divided into three:

  - **Pattern match for fauna**
  - **Pattern match for flora**
  - **Pattern match for citizen science**

### Pattern Match for Species Name (Fauna and Flora)

#### [`pattern_match.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/pattern_match/pattern_match.R)

**1. Dataset Variables**

Before going any further, let’s first understand the variables of the
external data needed for the script to run properly. The dataset must
include these 5 variables:

  - *Nama.Latin* (latin name)
  - *kelas* (species class)
  - *Nama.Lokal* (local name)
  - *Jumlah* (the number of species observed)
  - *Pengamat* (Observer name)

**2. Load Necessary Datasets**

  - [`pattern_match.xlsx`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/pattern_match/pattern_match.xlsx):
    This contains the external data (fauna) to be paired with the local
    database and as the directory for the pairing and pattern match
    result.
  - [`pattern_match_flora.xlsx`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/pattern_match/pattern_match_flora.xlsx):
    This contains the external data (flora) to be paired with the local
    database and as the directory for the pairing and pattern match
    result.

**3. Stringsim Function**

The essential part of the process here is to use and manipulate
stringsim function to compare and determine similarity between entries
in a certain threshold.

  - `stringsim` function: stringsim computes pairwise string
    similarities between elements of character vectors a and b, where
    the vector with less elements is recycled. stringsimmatrix computes
    the string similarity matrix with rows according to a and columns
    according to b.

**4. Save the result**

The last part of this script is saving the result into
[`pattern_match.xlsx`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/pattern_match/pattern_match.xlsx)

**5. Pattern Match for Observer Name**

The last function is the same process but it analyzes observer name
instead of fauna and flora data. It will generate a list of observer
names with new entries from the dataset in `pattern_match.xlsx`.

### Patern Match for Individual Counts

#### [`pattern_match_ind_count.R`](https://github.com/robbybinsar/Konservasi_ANJ/blob/master/pattern_match/pattern_match_ind_count.R)

This R script will do pattern match on species names across assemblages
and merge them together with the individual counts provided in the
dataset. The result will be used for diversity measures analysis.
