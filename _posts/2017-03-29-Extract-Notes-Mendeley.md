---
title: "Extract Mendeley Notes Using R"
author: "Mathieu Bray"
date: "2017-03-29"
tags: [R, SQL]
layout: post
---


*Code for this analysis is availabile on [Github](https://gist.github.com/mathieubray/c7073f542f54b52d93247544521ad4a2)*

Thought I would offer up a quick post on something that had sidetracked me earlier this week, namely how to extract notes and annotations in [Mendeley](https://www.mendeley.com/) using R. Basically I was having a similar problem as Daniel Hynk [here](http://danielhnyk.cz/export-pdf-annotations-mendeley-csv-or-txt/), which he solved using Python. I too use Mendeley as a reference manager, which also has the handy feature of allowing users to add their own annotations and notes to their saved documents. While I have been generally satisfied with Mendeley, I was in a situation where I wanted to put together a table containing some information on a set of papers I had been reading, including, in particular, the notes I had been adding. Unfortunately, Mendeley does not offer a simple option for doing this (some information is available for export to .bib or .xml).

Anyway, Mendeley stores all the information about a user's account locally in an SQLite database. We can connect to this database and extract the information, including the notes. I'm not sure how much this tutorial will generalize to other reference management software suites, but I imagine the underlying mechanics are similar. This tutorial can also be treated as an introduction to database interaction using the `RSQLite` package, available from [CRAN](https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf).

As always, let's start by loading some handy libraries.


```r
library(RSQLite) # Database 
library(dplyr) # This really should be loaded by default always...
library(tidyr) # 'spread' function to change table from 'long' to 'wide' format 
```

We first need to connect to the database. [Mendeley Support](http://support.mendeley.com/customer/en/portal/articles/227951-how-do-i-locate-mendeley-desktop-database-files-on-my-computer-) lists how to find the local database path for each system. In Windows, the `mendeley.path`, as we will refer to it in R, will look something like `C:/Users/{username}/AppData/Local/Mendeley Ltd./Mendeley Desktop/{youremail}@{emailclient.com}@www.mendeley.com.sqlite`.





```r
# Connect to the database
mendeley.connection = dbConnect(RSQLite::SQLite(),mendeley.path)
```

The Mendeley database contains a variety of tables. These can be listed by using the `dbListTables` funtion


```r
# Some of the tables available in the Mendeley database
head(dbListTables(mendeley.connection),n=10)
```

```
##  [1] "CanonicalDocuments"   "DataCleaner"          "DocumentCanonicalIds"
##  [4] "DocumentContributors" "DocumentDetailsBase"  "DocumentFields"      
##  [7] "DocumentFiles"        "DocumentFolders"      "DocumentFoldersBase" 
## [10] "DocumentKeywords"
```

For each table, to get an idea of the contents, the list of variable names can be printed using the `dbListFields` function, illustrated here for the "Documents" table. 


```r
# The variables available in the 'Documents' table
dbListFields(mendeley.connection,"Documents")
```

```
##  [1] "id"                       "uuid"                    
##  [3] "confirmed"                "deduplicated"            
##  [5] "deletionPending"          "favourite"               
##  [7] "read"                     "type"                    
##  [9] "abstract"                 "added"                   
## [11] "modified"                 "importer"                
## [13] "note"                     "privacy"                 
## [15] "title"                    "advisor"                 
## [17] "articleColumn"            "applicationNumber"       
## [19] "arxivId"                  "chapter"                 
## [21] "citationKey"              "city"                    
## [23] "code"                     "codeNumber"              
## [25] "codeSection"              "codeVolume"              
## [27] "committee"                "counsel"                 
## [29] "country"                  "dateAccessed"            
## [31] "day"                      "department"              
## [33] "doi"                      "edition"                 
## [35] "genre"                    "hideFromMendeleyWebIndex"
## [37] "institution"              "internationalAuthor"     
## [39] "internationalNumber"      "internationalTitle"      
## [41] "internationalUserType"    "isbn"                    
## [43] "issn"                     "issue"                   
## [45] "language"                 "lastUpdate"              
## [47] "legalStatus"              "length"                  
## [49] "medium"                   "month"                   
## [51] "originalPublication"      "owner"                   
## [53] "pages"                    "pmid"                    
## [55] "publication"              "publicLawNumber"         
## [57] "publisher"                "reprintEdition"          
## [59] "reviewedArticle"          "revisionNumber"          
## [61] "sections"                 "seriesEditor"            
## [63] "series"                   "seriesNumber"            
## [65] "session"                  "shortTitle"              
## [67] "sourceType"               "userType"                
## [69] "volume"                   "year"
```

I use the `extract.table` function defined below to effectively combine the actions of sending a database query (`dbSendQuery`), fetching the results (`dbFetch`), and freeing up the resources (`dbClearResult`). 



```r
extract.table <- function(con,query){
  
  res <- dbSendQuery(con,query) # Send query
  
  table <- dbFetch(res) # Fetch table
  
  dbClearResult(res) # Free resources
  
  return(table)
  
}
```

In Mendeley, saved documents can be sorted into one or more user-defined folders. In my particular case, I was focusing on a set of recent papers I have been reading on the topic of [multilayer and dynamic network analysis](https://en.wikipedia.org/wiki/Multidimensional_network), which I had sorted into a folder appropriately named "Networks". Based on the collection of tables shown earlier, it seems the "Folders" and "DocumentFolders" tables might come in handy. Note that I will use a mix of SQL commands and `dplyr` functions to get my desired results going forward.


```r
dbListFields(mendeley.connection,"Folders")
```

```
##  [1] "id"                  "uuid"                "name"               
##  [4] "parentId"            "access"              "syncPolicy"         
##  [7] "downloadFilesPolicy" "uploadFilesPolicy"   "publicUrl"          
## [10] "description"         "creatorName"         "creatorProfileUrl"
```

```r
folders <- extract.table(mendeley.connection, "SELECT id, name FROM Folders")

dbListFields(mendeley.connection,"DocumentFolders")
```

```
## [1] "documentId" "folderId"   "status"
```

```r
document.folders <- extract.table(mendeley.connection, "SELECT folderId, documentId FROM DocumentFolders")
```

We first need to retrieve the internal ID assigned to the "Networks" folder, then extract the IDs associated with all documents in that folder. 


```r
relevant.folder.name <- "Networks"

# Extract interal ID for folder of interest
relevant.folder.id <- (folders %>%
  filter(name == relevant.folder.name))$id[1]

# Extract internal IDs for all papers belonging to the folder of interest, using the folder ID
relevant.papers <- (document.folders %>%
  filter(folderId == relevant.folder.id))$documentId

head(relevant.papers)
```

```
## [1] 213 215 224 232 249 264
```


The "Documents"" table contains the document title, among others (for illustration, I also collect the citation key assigned to each document). Here we simply need to extract the table, filtering to include only those documents of interest. 



```r
# Collect title and citation key for all relevant documents
relevant.documents <- extract.table(mendeley.connection,"SELECT id, citationKey, title FROM Documents") %>% 
  filter(id %in% relevant.papers) %>%
  rename(documentId = id)
```


```
##   documentId   citationKey                                       title
## 1        213 Junuthula2016 Evaluating Link Prediction Accuracy in D...
## 2        215    Cortes2003 Computational methods for dynamic graphs...
## 3        224      Paul2016 Null models and modularity based communi...
## 4        232   Durante2016 Bayesian Learning of Dynamic Multilayer ...
## 5        249         Bazzi Generative Benchmark Models for Mesoscal...
## 6        264    Oselio2014 Multi-Layer Graph Analysis for Dynamic S...
```

Each document can have multiple authors, which are stored in the "DocumentContributors" table. Let's take a quick peak at the raw table.


```r
dbListFields(mendeley.connection,"DocumentContributors")
```

```
## [1] "id"           "documentId"   "contribution" "firstNames"  
## [5] "lastName"
```

```r
# Collect and concatenate authors for all relevant documents
authors <- extract.table(mendeley.connection,"SELECT * FROM DocumentContributors")

head(authors)
```

```
##   id documentId   contribution  firstNames  lastName
## 1  7          2 DocumentAuthor   Andrew C.    Thomas
## 2  8          3 DocumentAuthor Genevera I.     Allen
## 3  9          4 DocumentAuthor  Francis L. Delmonico
## 4 10          4 DocumentAuthor     Paul E. Morrissey
## 5 11          4 DocumentAuthor   George S. Lipkowitz
## 6 12          4 DocumentAuthor  Jeffrey S.     Stoff
```

```r
unique(authors$contribution)
```

```
## [1] "DocumentAuthor" "DocumentEditor"
```

The contribution field specifies whether the entry refers to an author or an editor. We reduce the table to our documents of interest and filter out the editors. To concatenate all of the authors into one string, we first concatenate the `lastName` and `firstName` using the `paste` function, then group by document and collapse each of the authors into one string, separated by a semi-colon (;), again using the `paste` funtion with the `collapse` option set.


```r
# Collect and concatenate authors for all relevant documents
relevant.authors <- authors %>%
  filter(contribution == "DocumentAuthor",
         documentId %in% relevant.papers) %>%
  mutate(fullName = paste(lastName,firstNames,sep=", ")) %>% # Concatenate first and last name
  select(documentId,fullName)
```


```
##   documentId               fullName
## 1        213  Junuthula, Ruthwik R.
## 2        213           Xu, Kevin S.
## 3        213 Devabhaktuni, Vijay K.
## 4        215        Cortes, Corinna
## 5        215        Pregibon, Daryl
## 6        215        Volinsky, Chris
```


```r
relevant.authors <- relevant.authors %>%
  group_by(documentId) %>%
  summarize(authorsNames = paste(fullName,collapse="; ")) # Concatenate all authors of a document together
```


```
## # A tibble: 6 ? 2
##   documentId                                authorsNames
##        <int>                                       <chr>
## 1        213 Junuthula, Ruthwik R.; Xu, Kevin S.; Dev...
## 2        215 Cortes, Corinna; Pregibon, Daryl; Volins...
## 3        224             Paul, Subhadeep; Chen, Yuguo...
## 4        232 Durante, Daniele; Mukherjee, Nabanita; S...
## 5        249 Bazzi, Marya; Jeub, Lucas G. S.; Arenas,...
## 6        264 Oselio, Brandon; Kulesza, Alex; Hero, Al...
```

The tags associated with each document can be extracted and concatenated in a similar manner to the authors. Tags are located in the "DocumentTags" table.


```r
dbListFields(mendeley.connection,"DocumentTags")
```

```
## [1] "documentId" "tag"
```

```r
# Collect and concatenate tags for all relevant documents
relevant.tags <- extract.table(mendeley.connection, "SELECT * FROM DocumentTags") %>%
  filter(documentId %in% relevant.papers) %>%
  group_by(documentId) %>%
  summarize(tagList = paste(tag,collapse="; "))
```


```
## # A tibble: 6 ? 2
##   documentId                                     tagList
##        <int>                                       <chr>
## 1        213 dynamic network; edge addition; link pre...
## 2        215 dynamic network; edge addition; node add...
## 3        224 Newman-Girvan null model; SBM; community...
## 4        232      dynamic network; multilayer network...
## 5        249 Multilayer networks; community detection...
## 6        264 dynamic SBM; dynamic network; multilayer...
```

Finally, we collect the document notes, located in the `FileNotes` table. In general, these would be stored similarly to tags, with one note per row and document ID specified, and can therefore be treated in the same manner as the tags to obtain a single string for each document. In my case, I had been annotating each paper in this folder with two notes, one for the main goal of each paper, introduced by "Goal:", and one for the key insights or techniques used in the article, introduced by "Key:". I classified each note into a `type` based on whether it represented a "Goal" or "Key" by splitting the note string by colon (:) and inspecting the first token.


```r
dbListFields(mendeley.connection,"FileNotes")
```

```
##  [1] "id"           "author"       "uuid"         "documentId"  
##  [5] "fileHash"     "page"         "x"            "y"           
##  [9] "note"         "modifiedTime" "createdTime"  "unlinked"    
## [13] "baseNote"     "color"        "profileUuid"
```

```r
# Collect notes
relevant.notes <- extract.table(mendeley.connection,"SELECT documentId, note FROM FileNotes") %>%
  filter(documentId %in% relevant.papers) %>%
  rowwise() %>%
  mutate(type = tolower(trimws(unlist(strsplit(note,split=":")))[1]), # Extract for each document whether it is a 'Goal' or a 'Key'
         note = paste(unlist(strsplit(note,split=":"))[-1])) %>% # Extract the actual note
  ungroup() 
```


```
## # A tibble: 6 ? 3
##   documentId                                        note  type
##        <int>                                       <chr> <chr>
## 1        417  Embed subgraphs into low dimensional co...  goal
## 2        417  Use NLP techniques. Treat each node as ...   key
## 3        384  Developed null models, multilayer modul...   key
## 4        249  Community detection in multilayer netwo...  goal
## 5        384  Community detection in temporal network...  goal
## 6        249  Generate random multilayer partitions w...   key
```

Finally, to tidy the data such that each document is on its own row, I use the `spread` function in `tidyr`, which creates an individual variable for each `type`, whose entry for each document contains the relevant note.


```r
relevant.notes <- relevant.notes %>%
  spread(type,note)
```


```
##   documentId                              goal
## 1        213  Study the link prediction pro...
## 2        215  Overview of computational met...
## 3        224  Community detection in multil...
## 4        232  Flexible model fo dynamic mul...
## 5        249  Community detection in multil...
## 6        264  Estimate the true adjacency m...
##                                 key
## 1  Previously, rolling 1-step fo...
## 2                             NA...
## 3  Newman-Girvan modularity or e...
## 4  Characterize edges as conditi...
## 5  Generate random multilayer pa...
## 6  Latent variable model which d...
```

At this point, We have collected all of our desired information, and it's a simple matter of joining all the tables together, using `documentId` as the primary key.


```r
# Join the datasets together
relevant.files <- relevant.documents %>%
  left_join(relevant.authors, by="documentId") %>%
  left_join(relevant.tags, by="documentId") %>%
  left_join(relevant.notes, by="documentId")
```


```
##   documentId   citationKey                             title
## 1        213 Junuthula2016 Evaluating Link Prediction Acc...
## 2        215    Cortes2003 Computational methods for dyna...
## 3        224      Paul2016 Null models and modularity bas...
## 4        232   Durante2016 Bayesian Learning of Dynamic M...
## 5        249         Bazzi Generative Benchmark Models fo...
## 6        264    Oselio2014 Multi-Layer Graph Analysis for...
##                        authorsNames                           tagList
## 1 Junuthula, Ruthwik R.; Xu, Kev... dynamic network; edge addition...
## 2 Cortes, Corinna; Pregibon, Dar... dynamic network; edge addition...
## 3   Paul, Subhadeep; Chen, Yuguo... Newman-Girvan null model; SBM;...
## 4 Durante, Daniele; Mukherjee, N... dynamic network; multilayer ne...
## 5 Bazzi, Marya; Jeub, Lucas G. S... Multilayer networks; community...
## 6 Oselio, Brandon; Kulesza, Alex... dynamic SBM; dynamic network; ...
##                                goal                               key
## 1  Study the link prediction pro...  Previously, rolling 1-step fo...
## 2  Overview of computational met...                             NA...
## 3  Community detection in multil...  Newman-Girvan modularity or e...
## 4  Flexible model fo dynamic mul...  Characterize edges as conditi...
## 5  Community detection in multil...  Generate random multilayer pa...
## 6  Estimate the true adjacency m...  Latent variable model which d...
```

The final data frame is ready for any kind of fancy analysis, or can be exported to CSV format (or whatever format you prefer).

That's basically it for now, but check back soon! I do plan on having some more interesting posts coming up shortly...
