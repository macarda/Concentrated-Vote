library(shiny)
library(ggplot2)
library(scales)
library(DT)
library(grid)
library(gridExtra)
library(shinythemes)
library(cowplot)
library(caTools)

ui <- (fluidPage(theme=shinytheme("journal"), navbarPage(title = "Visualisations of a Novel Electoral System",
        tabPanel(title = "About",
          h3("What is the \"Concentrated Vote\" Electoral System?"),
          column(5,tabsetPanel(
            tabPanel("Overview",
                h5("Background"),
                p("Majoritarian electoral systems are the oldest and simplest systems in use in democratic states and, despite a number of notable drawbacks, the most popular. They can be described as \"winner takes all\" and produce outcomes that reflect the views of the majority of a population, even where the majority is a small one. Majority systems are often criticised for producing results that disproportionately favour certain political parties and results that are unrepresentative of the whole of a country's voting population. For example, in the UK, which uses the majoritarian first past the post (FPTP) electoral method, the results of the 2015 general election were widely described as the most disproportionate in history. In the UK context, these criticisms are notable as they come only four years after the UK voted to reject the Alternative Vote, a proportional system, which would have replaced FPTP."),
                h5("The Concentrated Vote Electoral System"),
                p("The Concentrated Vote is an unpublished voting system developed by Dr. David Sterratt at the University of Edinburgh. Concentrated Vote is an alternative voting system that has been applied to the UK general election data from 1979-2017 as part of an MSc project. The aim of the Concentrated Vote electoral system is to address the issue of disproportionality with the FPTP system, currently used in the UK, while maintaining its simplicity for voters. Votes are cast in the same way as the FPTP system, with voters required to select a single candidate in a constituency. Votes are then counted to give an array of votes per party in each constituency but no candidates are elected until all votes in the country or region have been counted.")
            , p("The system works by first eliminating the candidate with the smallest number of votes in a given constituency and then shifting these votes to a constituency where the eliminated candidate's party has more votes. The process works by eliminating candidates one-by-one, which leads to a concentration of votes where a party started out strong.")
            ,h5("The Graphic")
            , p("The graphic on the right demonstrates this process of seats changing as votes are shifted between constituencies When all but a single candidate has been eliminated in a constituency, the candidate is then elected. At every stage of this process, the number of votes per constituency and per party remains unchanged. The process ends when there is no candidate that can be eliminated without breaking one of the constraints relating to the number of votes per constituency and per party.")
            , p("This MSc project investigates the extent to which the results produced by FPTP are disproportionate and explores whether the Concentrated Vote electoral system produces results that are proportional and appear reasonable to. All visualisations can be viewed for the election years 1979-2017 using the dropdown menu on the top right corner of the screen, except for the maps. The maps are only available for the years 2010, 2015, and 2017 as the constituency boundaries were different in years previous to this.")
            ),
            tabPanel("Further Details",withMathJax(),
                     h5("Technical Description"),
                     p("Technically, this problem can be formulated as a constrained quadratic optimisation problem. \\(V_{ij}\\) is the number of votes for party \\(j\\) in constituency \\(i\\). These votes will be transferred between candidates to give \\(X_{ij,k}\\) votes for party \\(j\\) in constituency \\(i\\) at time stage \\(k\\). The aim of the system is to transfer votes between constituencies to make them as distinct from each other as possible, while keeping the  number of votes per party and per constituency constant. This is formulated as follows:"),
                p("maximise \\(\\frac{1}{2}\\sum_{i}\\sum_{j}X^2_{ij,k}\\)    (1)",align = "center"),
                p("subject to \\(\\sum_{j}X_{ij,k} = \\sum_{j}V_{ij}\\)    (2)",align = "center"),
          p("\\(\\sum_{i}X_{ij,k} = \\sum_{i}V_{ij}\\)    (3)", align="center"),
          p("\\(X_{ij,k} \\geq 0\\)    (4)", align="center"),
          p("The constraint in equation 2 maintains the number of votes per constituency, equation 3 maintains the number of votes per party and equation 4 ensures the number of votes for each candidate in each seat remain positive."),
          p("Solving this optimisation problem is NP-hard, so a plausible, possibly sub-optimal, solution can instead be implemented. Lagrange multipliers are used to solve this problem. The method incorporates the maximisation function and the constraints into a Lagrange function, \\(L\\), in such a way that the extreme value is obtained only when the constraints are satisfied. The Lagrange Multiplier theorem allows the translation of the original constrained optimisation problem into a system of simultaneous equations, at the expense of adding an additional variable per constraint."),
          p("This can be formulated by introducing the binary variable \\(R_{ij}\\), which equals 1 if party \\(j\\) is still in the running for a seat in constituency \\(i\\), or 0 if they have been eliminated. The total constituency votes are represented by \\(c_i = \\sum_{j}R_{ij}X_{ij,k} = \\sum_{j}V_{ij}\\) and similarly, the total party votes are represented by \\(p_j = \\sum_{i}R_{ij}X_{ij,k} = \\sum_{i}V_{ij}\\). "),
          p("\\(R\\) is added to the objective function in equation 1 and the constraints in equations 2 and 3 will be replaced with the following constraints:"),
          p("\\(g_l(X) = \\sum_{j}R X_{lj} - c_l = 0\\)    (5)", align="center"),
          p("\\(h_m(X) = \\sum_{i}R X_{im} - p_m = 0\\)    (6)", align="center"),
          p("This system can be defined compactly as the Lagrangian, \\(L\\), with  multipliers \\(\\lambda_l\\) and \\(\\mu_m\\) for constituencies and parties respectively:"),
          p("\\(L(X) =\\frac{1}{2} \\sum_{i}\\sum_{j} R X^2_{ij} - \\sum_{l}\\lambda_{l} g_l(X) - \\sum_{m}\\mu_{m} h_m(X)\\)    (7)",align = "center"),
          p("with the additional constraint:"),
          p("\\(\\sum_{m}\\mu_{m} = 0\\)    (8)", align="center"),
          p("This constraint is required to ensure that there is a unique solution for the multipliers."),
          p("The gradient of \\(L\\) with respect to \\(X_{ij}\\) is:"),
          p("\\(\\frac{\\partial L}{\\partial X_{ij}} = R(X_{ij} - \\lambda_i - \\mu_j)\\)    (9)", align="center"),
          p("From this, we want the direction of the gradient of the Lagrangian, \\(L\\), to be perpendicular to the normal of the constraint. Solving equation 10, equation 11 can be derived to achieve this for the constituency constraints."),
          p("\\(\\sum_{i} \\sum_{j} \\frac{\\partial L}{\\partial X_{ij}} \\cdot \\frac{\\partial g_l}{\\partial X_{ij}} = 0\\)    (10)", align="center"),
          p("\\(c_l - \\lambda_l \\sum_m R_{lm} - \\sum_m \\mu_m R_{lm} = 0\\)    (11)", align="center"),
          p("Following the same analogy, equation 12 can be derived for the party constraints."),
          p("\\(p_m - \\sum_l \\lambda_l R_{lm} - \\mu_m \\sum_l R_{lm} = 0\\)    (12)", align="center"),
          p("To solve this system of equations for \\(\\lambda_{l}\\) and \\(\\mu_m\\), matrix inversion is employed as follows. Equations 8, 11, and 12 are combined in a matrix equation:"),
          p("\\(\\left( \\begin{matrix} \\boldsymbol{L} & \\boldsymbol{R-1} \\cr \\boldsymbol{R^{T}}&\\boldsymbol{M} \\end{matrix} \\right) \\left( \\begin{matrix} \\overrightarrow \\lambda \\cr \\overrightarrow \\mu \\end{matrix} \\right)\\)     (13)", align="center"),
          p("where \\(\\boldsymbol{L}\\) is a diagonal matrix whose non-zero elements are \\(L_{ii} = \\sum_{j}R_{ij}\\), \\(\\boldsymbol{M}\\) is a diagonal matrix whose non-zero elements are \\(M_{jj}=\\sum_{i}R_{ij}\\), \\(\\boldsymbol{R}\\) equals \\(R_{ij}\\), \\(\\boldsymbol{1}\\) is a matrix of the same size as \\(\\boldsymbol{R}\\) with every element equal to 1, and \\(\\overrightarrow \\lambda\\) and \\(\\overrightarrow \\mu\\) refer to \\(\\lambda_{l}\\) and \\(\\mu_m\\) respectively."),
          p("The candidacy \\(ij\\) for which \\(X_{ij}=0\\) is found when the direction of the gradient, obtained above, is followed, by computing \\(s_{ij} = \\frac{-X_{ij}}{\\frac{\\partial L}{\\partial X_{ij}}}\\) and setting \\(s\\) to be the minimum value of \\(s_{ij}\\). For this \\(ij\\), \\(X_{ij}=0\\) and \\(R_{ij}\\) are set and these calculations are repeated. If the gradient equals zero, within a tolerance, the process will end. This ensures the positivity of votes without explicitly including the constraint.")
          )))
            ,
          column(5,h4("Concentrated Vote Process Transfering Votes Between Constituencies"),imageOutput("dynmap")
              ),#column(5,imageOutput("dynmap"))),#column(5,tableOutput('tableinfo')),
                column(2,
                   wellPanel(selectInput("yeargif", "Choose a General Election year:",
                    list("2017","2015","2010"))
                  )
                )    
          ),
        tabPanel(title = "Wasted Votes",
                 h3("Wasted Votes"),
                 column(7,p("In the first past the post system, wasted votes are votes cast for losing candidates or votes cast for winning candidates in excess of the number required for victory. In the Concentrated Vote system, wasted votes are those left over when the process of elimination cannot continue without breaking one of the constraints relating to the number of votes per constituency and per party."),
                        plotOutput("wastedvotes")),
                 column(5,h4("Details of Wasted Votes"),dataTableOutput('tablewv'))
        ),
                 tabPanel(title = "Votes per MP Elected",
                          h3("Votes per MP Elected"),
                          value="mpelect",
                          column(10,#tabsetPanel(
                            #tabPanel("Standard", 
                            p("The number of votes required for an MP to be elected per party. The first past the post system favours large parties, making it difficult for smaller parties to win seats. The Concentrated Vote system allows all parties to have a more equal opportunity of winning a seat."),
                            
                                     plotOutput("MPelect")#),
                           # tabPanel("Normalised", h5("Constituency sizes have been normalised to allow each constituency equal voting influence regardless of varying population sizes."),plotOutput("MPelect100"))#,
                            #tabPanel("Fewer Parties", plotOutput("MPelectlp"))
                          ),column(2,
                                   wellPanel(selectInput("year", "Choose a General Election year:",
                                                         list("2017","2015","2010","2005","2001","1997","1992","1987","1983","1979"))
                                   )
                                  
                 )),
                tabPanel(title = "Vote/Seat Share",
                         h3("Vote Share versus Seat Share"),
                         fluidRow(column(5,p("The aim of first past the post is to create an exaggerated share of seats in the national parliament for the leading party in order to produce an effective parliamentary majority for the government. In order to be elected, a candidate simply needs to receive more votes than the opposition candidates in their constituency, which leads to a striking difference between a party's share of the vote versus their share of parliamentary seats.")
                                  ),column(5, p("The primary criticism is that the results are not representative of the population as a whole. Candidates do not need to meet a minimum threshold of votes or an absolute majority to be elected; they simply receive more votes than their opponents. The Concentrated Vote system aims for the distribution of views among those who are elected to resemble the distribution of views of the electorate."))
                                  ,column(2,
                                          wellPanel(selectInput("yearshare", "Choose a General Election year:",
                                                                list("2017","2015","2010","2005","2001","1997","1992","1987","1983","1979"))
                                          )
                                  )),
                         fluidRow(column(5,plotOutput("share")),
                         column(5,plotOutput("pchange"))
                         )#,
                        # fluidRow(
                          # column(10,plotOutput("pchange"))
                        # )
                ),
                 tabPanel(title = "Result Changes",
                          h3("Changes to First Past the Post Seats in the Concentrated Vote Electoral System"),
                          column(5,
                                 p("In order to be elected in the first past the post system, a candidate simply needs to receive more votes than the opposition candidates in their constituency. In contrast, the Concentrated Vote system transfers votes between constituencies, lending to a more proportional result. This means the candidate in a constituency with the most votes is not necessarily always elected."),
                                 p("The histogram below demonstrates where the candidates elected by the Concentrated Vote system originally ranked in their constituencies based on the first past the post results. Where a candidate is ranked first, they received the most votes in their constituency and the same candidate is elected in both systems. Where the Concentrated Vote has not elected the candidate with the most votes, the position the candidate originally ranked is recorded in the histogram and information relating to the constituency and the original votes received will display in the table on the right."),
                                 plotOutput("rank")),
                          column(5,h4("Constituencies where the Elected MP has Changed"),dataTableOutput('tablerank')),
                          column(2,
                                 wellPanel(selectInput("yearrank", "Choose a General Election year:",
                                                       list("2017","2015","2010","2005","2001","1997","1992","1987","1983","1979"))
                                 )
                          )
                 ),
              tabPanel(title=" Proportional Maps",
                 h3("Cartograms of UK General Election Results"),
                column(6,p("A cartogram is a map in which geographic region sizes appear in proportion to their population and are particularly relevant for election data as on an ordinary geographic map constituency size is not proportional to its population."),
                        # tabsetPanel(
                        # tabPanel("Proportional", 
                        plotOutput("propmap")),
                 # tabPanel("Dynamic", imageOutput("dynmap"),
                 #tabPanel("Geographical", plotOutput("geomap")
                 column(4,h4("Changes to Party Seats"),dataTableOutput('tableinfo')),
                 column(2,
                        wellPanel(selectInput("yearmap", "Choose a General Election year:",
                                              list("2017","2015","2010"))
                        ))
              ),
                 tabPanel(title = "Constituency Sizes",
                          h3("Constituency Sizes for Conservative and Labour Seats"),
                          column(4, p("Electoral bias relating to the constituency sizes is a well known issue in the UK. It is often claimed by politicians and political commentators that the current constituency boundaries are biased against the Conservatives in favour of Labour. The Conservatives tend to win seats in larger constituencies while Labour tend to win seats in smaller constituencies with a lower voter turnout."),
                                 p("Boxplots were used to investigate the constituency sizes of Conservative and Labour seats wins under first past the post and Concentrated Vote systems. A boxplot is a method of graphically representing groups of numerical data through their quartiles. Five values from the data are depicted in a boxplot: the upper and lower quartiles, represented by the top and bottom of the box, the median representing by the middle line in the box, the extreme non-outlier values, represented by the vertical lines from the box, and outliers represented by the data points outside these ranges."),
                                 p("There is a strong connection between Conservative seats wins and larger constituencies and Labour seat wins and smaller constituencies. The Concentrated Vote successfully reduces this electoral bias, as Conservative and Labour contain roughly similar values and the size of a constituency does not appear to influence seat wins to the same extent.")),
                          column(6,plotOutput("box")),
                          column(2,
                                 wellPanel(selectInput("yearbox", "Choose a General Election year:",
                                                       list("2017","2015","2010","2005","2001","1997","1992","1987","1983","1979"))
                                 )
                          )
                )                                   
)))

server = function(input, output) {
  source("source/servermap.R", local = TRUE)
  source("source/servergif.R", local = TRUE)
  source("source/servermpelect.R", local = TRUE)
  source("source/servervoteseatshare.R", local=TRUE)
  source("source/serverseatchangerank.R", local=TRUE)
  source("source/serverwastedvotes.R", local=TRUE)
  source("source/serverconstitplots.R", local=TRUE)
}

shinyApp(ui = ui, server = server)