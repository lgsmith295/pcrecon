             This folder contains the data you requested via the          
   NOAA World Data Service for Paleoclimatology Paleo Data Search Web Service     


Folder Contents
-------------------

README.txt - this file

manifest.json - The manifest is a special file that contains information about 
the datafiles which match the search parameters. Note: in some cases, the actual
datafiles will not be downloaded, and only metadata needed for access will be 
provided (when the dataset provider is not NOAA World Data Service for 
Paleoclimatology, or for a few large or complex studies). 
The manifest.json file contains four sections:

     o files
       One entry per file in the zip archive.
       This section lists the contents of the "data" sub-folder of the archive, 
       including the path to each file, file size in bytes, and the MD5 checksum
       value of the file. 

     o params
       The search parameters used to find the data files in this archive.

     o matches
       One entry per file that matched the search params, including duplicates
       and non-downloaded files.
       This section is made up of a list of JSON metadata entries, one entry 
       for each datafile which matched your search parameters. Each metadata 
       entry includes: 
       the URL of the datafile; the Study Name (title) of the dataset the 
       datafile is a part of; a "download" flag which indicates whether or not 
       the datafile is contained in the "data" sub-folder; URL of full JSON 
       metadata for the datafile and dataset which contains it; and other 
       descriptive information. 

     o info
       Provides information about the data archive, including the time of 
       creation, and refers the reader to this README.txt file for more details. 

data - Sub-folder which contains the actual dataset files.

metadata - Sub-folder which contains a metadata document for each study matched by the submitted search parameters 

Use Constraints
---------------
Please cite original publication, onlineResourceLink and date accessed when 
using these data. If there is no publication information, please cite 
investigator, title, online resource and date accessed. All of this 
information is located in the JSON Metadata record associated with each data 
file in the manifest.txt file section "one entry per file that matched the 
search params, including duplicates and non-downloaded files".


Distributor
-----------
National Centers for Environmental Information, NESDIS, NOAA, U.S. Department of Commerce


Contacts
--------

     o For information about the Paleo Search API, Search Help, and 
       capabilities in general visit
       <https://www.ncdc.noaa.gov/paleo-search/>

     o To contact NOAA WDS Paleoclimatology email: paleo@noaa.gov