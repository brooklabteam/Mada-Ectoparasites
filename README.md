# Mada-Ectoparasites

This is a github repository for Angelo's ectoparasite work for the Coding for Conservation project. The github is organized by folder, corresponding to the names of each folder. The 'figure-development' folder has a script to produce each of the final figures and supplementary figures for the manuscript. All data for the paper is included in the 'data' folder, with a special subfolder labeled 'climate' that includes the climate data described under the subheader below.

#   Climate data for ectoparasite collection sites
Climate and vegetation greenness data was downloaded from NASA Earthdata using the Giovanni tool (https://giovanni.gsfc.nasa.gov/giovanni/). Raw climate data includes monthly time averaged maps of precipitation (TRMM: The Tropical Rainfall Measuring Mission: Near-Real-Time Precipitation Rate (TRMM_3B42RT v7, 3-Hourly, 0.25 degrees lat/lon), temperature (MERRA-2 MODEL: Surface Air Temperature (M2TMNXFLX V5.12.4, Monthly, 0.5x0.625 degrees lat/lon), and daytime humidity (AIRS: Relative Humdidity at Surface (Daytime/Ascending), AIRS-only, AIRX3STM v7.0, Monthly, 1 degree lat/lon), nighttime humidity (AIRS: Relative Humdidity at Surface (Nighttime/Descending), AIRS-only, AIRX3STM v7.0, Monthly, 1 degree lat/lon) and raw vegatation data includes monthly time averaged maps of NDVI (MODIS-TERRA, MOD13C2 v006, Monthly, 0.5 degrees lat/lon). 

# Building a Maximum Likelihood Tree For Ectoparasite Sequences

## COI-1 DNA Barcoding Phylogeny

All scripts and files associated with this process (to produce Figure S3 and Figure 5 in the final paper) are found in the 'phylogeny' subfolder, embedded within the 'prep-files' subfolder within the 'figure-development' main folder.

To prep the COI ML tree, we first made a list of background sequences to pull from GenBank, with representatives from each Genus in the ectoparasite tree. We compiled this list of sequences from the published literature, chiefly [Attaullah et al. 2023](https://link.springer.com/article/10.1007/s00436-023-07909-0), [Tortosa et al. 2013](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0075215), and [Poon et al. 2023](https://parasitesandvectors.biomedcentral.com/articles/10.1186/s13071-023-05663-x).  Here is the command you need to copy into the web browser to download these sequences (also found in the 'background_sequence.txt' file):

```
http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&rettype=fasta&retmode=text&id=KR997994,KR997992,KR997999,KR998001,MH151059,MH151060,KR997998,MH151062,LC536587,LC536586,LC536588,MH151064,MH151061,KR997993,MH151066,KR997997,KF021491,MH151065,MH151063,KR997996,KR997995,KF021500,KF021499,KF021496,KF021498,KF021497,KF021493,OM283592,OM283590,OM283588,OM283589,OM283591,OM283593,KF021495,KF021494,KF273783,MF462043,KF273782,KF273778,KF273770,OL847632,MH282032,KF021492,KF021517,MF462046,MK140156,MW590968,MK140160,KF021501,LC522026,LC522022,MW590972,MK140181,ON704710,ON704703,ON704664,KF021535,MK140180,MF462051,NC_001709,OM327589,OM327588,MK140116,MZ483872,AB632570,MT362948,AB632571,MT362949,MW792204,MW792205,MZ382456,KF021534,KF021518,MK140183,MT362947,AB632567,MK140104,AB632538,AB632536,KF273779
```

Above includes one sequence ("NC_001709") which is *Drosophila melanogaster* and meant to serve as a root to our tree.

We downloaded these sequences to produce the file "Ecto_Background_Sequences.fasta". It required some manual editing using 'Find and Replace' keys to replace all spaces and odd characters (",", ";", "/", "(", ")", ":", and so on) with underscores to yield the file stored here.

Next, we combined the reference file with our own cleaned consensus sequence files to produce "Ecto_All_Seq_Combined_Drosophila_Removed.fasta". This file has already been cleaned to make sure that no sequences are reverse complement of those found in GenBank.

Then, we aligned "Ecto_All_Seq_Combined_Drosophila_Removed.fasta" using the [MAFFT](https://mafft.cbrc.jp/alignment/server/) algorithm in the program Geneious Prime, but this could be done on the web browser, in MEGA, or a variety of other softwares.

We then visually checked the alignment file, and we downloaded two alignments which ultimately produced equivalent phylogenetic results. In the 'phylo-full' subfolder, we downloaded the raw alignment file: "AllEctoAlignedFull.fasta". Note that the *Drosophila melanogaster* root sequence in this file is full genome in length so considerably larger than all other sequences which corresponded to the COI gene for invertebrates.

In the 'phylo-trimmed' subfolder, we downloaded a version of the alignment which we trimmed to 336 conserved bps across all sequences (making an even end-to-end file): "AllEctoAligned_Trimmed.fasta". 

From above, we undertook parallel analyses in both the 'phylo-full' and 'phylo-trimmed' folders. For simplicity, we describe these processes in reference to the 'phylo-trimmed' folder here. 

We first sent the alignment file to  [modeltest-NG](https://github.com/ddarriba/modeltest) to determine the best nucleotide substitution model for the data. Both trimmed and full alignments produced the same results ('GTR+I+G4' model was favored). We ran modeltest remotely on the University of Chicago computing cluster, but it could also be run on your local device using the following command (after installing the program):


```
modeltest-ng -i path_to_folder/Mada-Ectoparasites/Phylogeny/phylo-trimmed/AllEctoAligned_Trimmed.fasta -d nt -t ml -p 8

```

where "path_to_folder" is your local path to this repository.

Results of modeltest-ng are found in the corresponding output folder within both "phylo-full" and "phylo-trimmed" subfolders within the main 'phylogeny' subfolder (in 'figure-development/prep-files').

Now that we understood the appropriate model for our data, we next tested that it could be read by [RaxML-NG](https://github.com/amkozlov/raxml-ng). This step first required installation of RaxML-NG. On the command line, we navigated to the folder corresponding to our alignment file and ran the following command:


```
/Applications/raxml-ng/raxml-ng --check --msa AllEctoAligned_Trimmed.fasta --model GTR+I+G4 --prefix T1
```

Here, "/Applications/raxml-ng/raxml-ng" should be replaced with whatever command runs the program based on the location of your installation.

Next, we parsed the parsing the alignment to find the appropriate number of threads (1) with which to run RAxML (either locally or on a server):

```
/Applications/raxml-ng/raxml-ng --check --msa AllEctoAligned_Trimmed.fasta --model GTR+I+G4 --prefix T2
```
Finally, we kicked off RAxML (including bootstraps) with the following script:

```


/Applications/raxml-ng/raxml-ng --check --msa AllEctoAligned_Trimmed.fasta --model GTR+I+G4 --prefix T3  --seed 12 --threads 1 --bs-metric fbp,tbe

```

Once RAxML finished (a few hours later for the full tree and ~45 min later for the trimmed tree), we importing the tree with bootstraps computed by [Felsenstein's method](https://www.jstor.org/stable/2461605) into R and built a phylogenetic tree for Fig 5. Note that for quick viewing, you can easily view the tree in [FigTree](http://tree.bio.ed.ac.uk/software/figtree/) and manually root it on the "NC_001709" *Drosophila* sequence. See the script, Fig5_S2.R in the "figure-development' subfolder for annotated instructions on how to build the tree visualized in our main paper.


## 18S DNA Barcoding Phylogeny

All scripts and files associated with this process (to produce Figure S3 and Figure 5 in the final paper) are found in the 'phylogeny' subfolder, embedded within the '18S' subfolder of the 'prep-files' subfolder within the 'figure-development' main folder.

Because no previous *Megastrebla wenzeli* sequences have been reported from Madagascar, and all existing *Megastrebla* spp. sequences correspond to the 18S gene, we additionally barcoded a subset of 18S sequences and built a different phylogeny with this focal gene.


To prep the 18S ML tree, we first made a list of background sequences to pull from GenBank, with representatives from each Genus in the ectoparasite tree. We compiled this list of sequences from the published literature, chiefly [Tortosa et al. 2013](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0075215), and [Dittmar et al. 2006](https://www.sciencedirect.com/science/article/pii/S1055790305002058?casa_token=bG0SlqbzqEsAAAAA:UJ2GIlFTwnCmghVm50VpXU_TmdUmj0YtPojquZq0EO7l525rsGpc3Y9RF5rkalZKjcVSU870qA).  Here is the command you need to copy into the web browser to download these sequences (also found in the 'background_sequence_18S.txt' file):

```
http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&rettype=fasta&retmode=text&id=DQ133083,DQ133077,DQ133060,DQ133062,DQ133054,DQ133056,DQ133085,DQ133066,DQ133055,DQ133072,DQ133080,DQ133082,DQ133063,DQ133079,DQ133061,DQ133069,DQ133053,DQ133070,DQ133088,DQ133084,DQ133081,DQ133087,DQ133065,DQ133086,DQ133071,DQ133057,DQ133064,DQ133073,DQ133076,DQ133068,DQ133058,DQ133078,AF073888,AF322421,AF322431,M21017,KF156670,KF156671,KF156672,KF156673,KF156674,KF156680,KF156675,KF156678,KF156681,KF156676,KF156677,KF156679,KF156683,KF156685,KF156684,KF156686,KF156687,KF156694,KF156691,KF156692,KF156693,KF156695,KF156690,KF156689,KF156688,KF156696,KF156682,KF156711,KF156703,KF156714,KF156707,KF156706,KF156708,KF156700,KF156709,KF156712,KF156713,KF156710,KF156705,KF156704,KF156702,KF156701,KF156697,KF156698   
```


Above includes one sequence ("M21017") which is *Drosophila melanogaster* and meant to serve as a root to our tree.

We downloaded these sequences to produce the file "Ecto_Background_Sequences_18S.fasta". It required some manual editing using 'Find and Replace' keys to replace all spaces and odd characters (",", ";", "/", "(", ")", ":", and so on) with underscores to yield the file stored here.

Next, we combined the reference file with our own cleaned consensus sequence files to produce "Ecto_All_Seq_Combined_18S.fasta". This file has already been cleaned to make sure that no sequences are reverse complement of those found in GenBank.

Then, we aligned "Ecto_All_Seq_Combined_18S.fasta" using the [MAFFT](https://mafft.cbrc.jp/alignment/server/) algorithm in the program Geneious Prime, but this could be done on the web browser, in MEGA, or a variety of other softwares.


From above, we undertook parallel analyses of both a full and a trimmed alignment. We visually checked the alignment file in Geneious to produce, "18Salign.fasta", and trimmed it to the length of the longest sequence in the set (388 conserved bps) to produce, "18Salign_trim.fasta". 

Next, we sent both alignment files to  [modeltest-NG](https://github.com/ddarriba/modeltest) to determine the best nucleotide substitution model for the data. Both trimmed and full alignments produced the same results ('TIM2+I+G4' model was favored for full and 'TPM2uf+I+G4' for the trimmed). We ran modeltest remotely on the University of Chicago computing cluster, but it could also be run on your local device using the following command (after installing the program):


```
modeltest-ng -i path_to_folder/Mada-Ectoparasites/Phylogeny/phylo-trimmed/18Salign_trim.fasta -d nt -t ml -p 8

```

where "path_to_folder" is your local path to this repository.

Results of modeltest-ng are found in the corresponding output folder within both "modtest-full" and "modtest-trim" subfolders within the '18S' subfolder of the main 'phylogeny' subfolder (in 'figure-development/prep-files').

Now that we understood the appropriate model for our data, we next tested that it could be read by [RaxML-NG](https://github.com/amkozlov/raxml-ng). This step first required installation of RaxML-NG. On the command line, we navigated to the folder corresponding to our alignment file and ran the following command:


```
/Applications/raxml-ng/raxml-ng --check --msa 18Salign_trim.fasta --model TPM2uf+I+G4 --prefix T1
```

Here, "/Applications/raxml-ng/raxml-ng" should be replaced with whatever command runs the program based on the location of your installation.

Next, we parsed the parsing the alignment to find the appropriate number of threads (1) with which to run RAxML (either locally or on a server):

```
/Applications/raxml-ng/raxml-ng --parse --msa 18Salign_trim.fasta --model GTR+I+G4 --prefix T2
```
Finally, we kicked off RAxML (including bootstraps) with the following script:

```


/Applications/raxml-ng/raxml-ng --check --msa 18Salign_trim.fasta --model GTR+I+G4 --prefix T3  --seed 12 --threads 1 --bs-metric fbp,tbe

```

Once RAxML finished (a few hours later for the full tree and ~45 min later for the trimmed tree), we importing the tree with bootstraps computed by [Felsenstein's method](https://www.jstor.org/stable/2461605) into R and built a phylogenetic tree for Fig 5. Note that for quick viewing, you can easily view the tree in [FigTree](http://tree.bio.ed.ac.uk/software/figtree/) and manually root it on the "NC_001709" *Drosophila* sequence. See the script, Fig5_S2.R in the "figure-development' subfolder for annotated instructions on how to build the tree visualized in our main paper.
