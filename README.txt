The files contained in this bundle were used to create the R Shiny interface as part of the MSc dissertation titled “Visualising the Results of a Novel Electoral System”. The “Shiny” folder contains the necessary files to run the R Shiny interface. The “Other” folder contains all files used in the creation of the interface. A short description of all files in both folders is given below.

“Other” Folder
Project.Rproj - The project file.
voting.R - Contains the Concentrated Vote algorithm in the shifter function (referred to as concentrator in dissertation).
run.R - Runs the Concentrated Vote algorithm for all electoral years between 1979 to 2017, with candidates below the cut-off removed and independent candidates separated. Northern Ireland and the rest of the UK are run separately.
connected.R - Finds “islands” of constituencies if they exist.
runConnected.R - Runs the Concentrated Vote algorithm, as above in run.R, but uses connected.R to find islands beforehand. If islands exist, the algorithm will be run separately on each.
runNormalised.R - Runs the Concentrated Vote algorithm, as above in run.R, but normalised constituencies to 100.
proportionalityMetrics.R - Calculates the LH, Gallagher, and Sainte-Lague Indices and look at marginal seat wins.
altsystems.csv - Used to calculate the Gallagher index for the comparison with alternative systems in chapter 4 of the report.
Data folder - Contains CSV files for the electoral data for the years 1979 to 2017.
Output folder - Contains CSV files of the output of the algorithm on the standard and normalised data for the years 1979 to 2017.
ShapeFiles folder - Contains the shape files for the construction of the proportional maps (obtained from Benjamin Hennig).
Create Visualisations folder - Contains the R files used to prepare data for the R Shiny visualisations.
readVotes.R - Reads and pre-processes the CSV files in the Data folder.
constitplots .R - Records the number of votes cast in the constituencies where the Conservatives and Labour party won seats under FPTP and Concentrated Vote.
seatchangerank .R - Creates a data frame of the rank of each of the Concentrated Vote seat winners in their constituency.
voteseatshare.R - Creates a data frames of the vote share, seat share, and percentage change between FPTP and Concentrated Vote seats for 1979-2017.
votespermp.R - Calculates the votes per MP elected for FPTP and Concentrated Vote.
votespermpnorm.R - Calculates the votes per MP elected for FPTP and Concentrated Vote on the normalised results.
wastedvotes.R - Calculates the wasted votes under FPTP and Concentrated Vote for all years and summarises wasted vote information for interface table.
proportionalMaps.R - Fortifies shape file of proportional map into a data frame and adds the FPTP and Concentrated Vote seat winners to the data frame.
Infotable.R - Creates the table on the “Proportional Maps” tab of the interface.
createGIF.R - Creates the animated GIFs displayed on the “About” tab of the interface.
gif folder - Contains CSV files used to create the animated GIF.

“Shiny” Folder
app.R - The application framework to run the interface, contains both the UI and server components.
source folder - Contains the server elements which prepare the visualisations.
serverconstitplots.R - Prepares the visualisations on the “Constituency Sizes” tab of the interface.
servergif.R - Prepares the GIF on the “About” tab of the interface.
servermap.R - Prepares the visualisations and tables on the “Proportional Maps” tab of the interface.
servermpelect.R - Prepares the visualisations on the “Votes per MP Elected” tab of the interface.
serverseatchangerank.R - Prepares the visualisations and tables on the “Result Changes” tab of the interface.
servervoteseatshare.R - Prepares the visualisations on the “Vote/Seat Share” tab of the interface.
serverwastedvotes.R - Prepares the visualisations and tables on the “Wasted Votes” tab of the interface.
data folder - Contains all files used to create the visualisations.