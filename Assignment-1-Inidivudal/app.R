# Load Packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(formattable)
library(plotly)

# Data cleaning

# Read in data
df <- read.csv(file = "DATA2x02 survey (Responses) - Form responses.csv")

# Rename the columns so they are more manageable
short_names <- c("timestamp", "covid_tests", "living_arrangments", "height", "wednesday_question", "in_au", "math_ability", "r_ability", "finding_data2x02", "uni_year", "zoom_webcam", "vaccination", "fav_social_media", "gender", "steak_preference", "dominant_hand", "stress_level", "loneliness_level", "emails", "email_sign_off", "data_scientist_salary", "in_advanced", "majors", "exercising_hours")
colnames(df) <- short_names

# Split the "Timestamp" column into "date" and "time" columns.
# NB: "date" indicates the date that the the person completed the survey.
# NB: "time" indicates the times that the person completed the survey on the specified day.
df <- df %>%
  separate(timestamp, c("date", "time"), sep = "\\s+", remove = FALSE) # "remove = FALSE" to keep the original "timestamp" column

# Clean up height column so that all height measurements are in cm.
#   On visual inspection, it appears the majority of people wrote cm as their height, but there were many variants:
#   i.e. (centimeters) need to clean for xcm, x cm, xcm<other characters>; where x is the height in cm.
#   i.e. (metres) need to clean for x m, xm, x M, xM, x metres; where x is the height in m.
#   i.e. (inches and feet) need to clean for x'n", x'n'', x' n", x' n'', x'n, x’n, xfoot ninches; where x is feet and n is inches
#   i.e. (random sentences or unreadable sequences) e.g. "Short enough to take offence at this question". These will be converted to NA.
#   i.e. (no unit specified) where a unit was not specified, the most natural unit will be assumed. For example, it is assumed a height of 5.5 is 5 feet and 5 inches,
#        as it is impossible to be 5.5 m or cm tall.
#   i.e. (multiple units specified) e.g. 168 cm (5 feet 5 inches)
#   i.e. (left blank) treated as NA.

# Function to convert feet and inches to cm.
# Values passed in are characters, and value passed out is numeric.
feet_and_inches_to_cm <- function(feet_string, inches_string) {
  feet = as.numeric(feet_string)
  inches = as.numeric(inches_string)
  inches = feet * 12 + inches
  cm = inches * 2.54
  return(cm)
}

# Function to convert a string representing a value in m to cm.
# cut_off is the number of characters to remove from the end of the string, so that the function converts only the numeric part of the string.
m_to_cm <- function(m_string, cut_off) {
  m = as.numeric(substr(m_string, 1, nchar(m_string)-cut_off))
  m = m * 100
  return(m)
}

# Loop through all heights, and convert them to numbers in cm.
for (i in 1:length(df$height)){
  
  current_height = df$height[i]
  
  # NB: It is possible to pool some of these if statements together, but there are no current performance issues to warrant such a change.
  
  # Matches form: xcm
  if (grepl("^[0-9]+(\\.[0-9]+)?cm$", current_height)) {
    df$height[i] = substr(current_height, 1, nchar(current_height)-2)
    
    # Matches form: x cm
  } else if (grepl("^[0-9]+(\\.[0-9]+)? cm$", current_height)) {
    df$height[i] = substr(current_height, 1, nchar(current_height)-3)
    
    # Matches form: xcm<other characters>
  } else if (grepl("^[0-9]+(\\.[0-9]+)?cm", current_height)) {
    df$height[i] = str_split(current_height, "cm")[[1]][1]
    
    # Matches form: xm or xM
  } else if (grepl("^[0-9]+(\\.[0-9]+)?(m|M)$", current_height)) {
    df$height[i] = m_to_cm(current_height, 1)
    
    # Matches form: x m or x M
  } else if (grepl("^[0-9]+(\\.[0-9]+)? (m|M)$", current_height)) {
    df$height[i] = m_to_cm(current_height, 2)
    
    # Matches form: x metres or x Metres or x meters or x Meters
  } else if (grepl("^[0-9]+(\\.[0-9]+)? (m|M)(etres|eters)$", current_height)) {
    df$height[i] = m_to_cm(current_height, 7) 
    
    # Matches form: x'n"  
  } else if (grepl("^[0-9]*'[0-9]*\"$", current_height)) {
    feet = str_split(current_height, "'")[[1]][1]
    inches = str_split(current_height, "'")[[1]][2]
    inches = substr(inches, 1, nchar(inches)-1)
    df$height[i] = feet_and_inches_to_cm(feet, inches)
    
    # Matches form: x'n'' 
  } else if (grepl("^[0-9]*'[0-9]*''$", current_height)) {
    current_height = substr(current_height, 1, nchar(current_height)-2)
    feet = str_split(current_height, "'")[[1]][1]
    inches = str_split(current_height, "'")[[1]][2]
    df$height[i] = feet_and_inches_to_cm(feet, inches)
    
    # Matches form: x' n"
  } else if (grepl("^[0-9]*' [0-9]*\"$", current_height)) {
    current_height = substr(current_height, 1, nchar(current_height)-1)
    feet = str_split(current_height, "' ")[[1]][1]
    inches = str_split(current_height, "' ")[[1]][2]
    df$height[i] = feet_and_inches_to_cm(feet, inches)
    
    # Matches form: x' n''
  } else if (grepl("^[0-9]*' [0-9]*''$", current_height)) {
    current_height = substr(current_height, 1, nchar(current_height)-2)
    feet = str_split(current_height, "' ")[[1]][1]
    inches = str_split(current_height, "' ")[[1]][2]
    df$height[i] = feet_and_inches_to_cm(feet, inches)
    
    # Matches form: x'n
  } else if (grepl("^[0-9]*'[0-9]*$", current_height)) {
    feet = str_split(current_height, "'")[[1]][1]
    inches = str_split(current_height, "'")[[1]][2]
    df$height[i] = feet_and_inches_to_cm(feet, inches)
    
    # Matches form: x’n
  } else if (grepl("^[0-9]*’[0-9]*$", current_height)) {
    feet = str_split(current_height, "’")[[1]][1]
    inches = str_split(current_height, "’")[[1]][2]
    df$height[i] = feet_and_inches_to_cm(feet, inches)  
    
    # Matches form: xfoot ninches
  } else if (grepl("^[0-9]*foot [0-9]*inches$", current_height)) {
    feet = str_split(current_height, " ")[[1]][1]
    inches = str_split(current_height, " ")[[1]][2]
    feet = substr(feet, 1, nchar(feet)-4)
    inches = substr(inches, 1, nchar(inches)-6)
    df$height[i] = feet_and_inches_to_cm(feet, inches)
    
    # Matches form: n cm <other text> 
  } else if (grepl("^[0-9]* cm", current_height)) {
    cm = str_split(current_height, " cm")[[1]][1]
    df$height[i] = cm
    
    # Matches form: no unit specified
    # Anything between (inclusive) 54.6 and 243 should be converted to cm.
    # Anything between 0.546 (inclusive) and 1.8 (exclusive) will be assumed to be m.
    # Anything between (inclusive) 1.8 and 2.43 could be feet or m, so will be converted to NA.
    # Anything between 2.43 (exclusive) and 7.97 (inclusive) will be assumed to be feet.
    # Anything left over will be converted to NA
    # These justifications are based upon how the tallest person ever is 243cm and the shortest person ever is 54.6cm.
    # Tallest person -> https://www.guinnessworldrecords.com/records/hall-of-fame/robert-wadlow-tallest-man-ever#:~:text=The%20twins%20then%20named%20Robert,8%20ft%2011.1%20in)%20tall
    # Shortest person -> https://www.guinnessworldrecords.com/news/2021/8/a-history-of-the-worlds-shortest-people-and-the-countries-theyre-from-668323
  } else if ( grepl("^[0-9]+\\.[0-9]*$", current_height) | grepl("^[0-9]+$", current_height)){
    value = as.numeric(current_height)
    if (value >= 54.6 & value <= 243) {
      df$height[i] = value
    } else if (value >= 0.546 & value < 1.8) {
      df$height[i] = value * 100
    } else if (value >= 1.8 & value <= 2.43) {
      df$height[i] = NA
    } else if (value > 2.43 & value <= 7.97) {
      df$height[i] = value * 30.48
    } else {
      df$height[i] = NA
    }
    
    # Anything else that hasn't been converted yet is not in an appropriate form.
  } else {
    df$height[i] = NA
  }
  
}

# Convert "height" column to numeric type.
df$height = as.numeric(as.character(df$height))

# There were some values which must have been input incorrectly, so now cleaning for these.
# e.g. someone said their height is 1.90cm.
# Remove values larger than 243 and smaller than 54.6
df = df %>%
  filter(height >= 54.6 & height <= 243 )

# Clean "wednesday_question" column so that all days of the week are capatilised, and so that days of the week are in a consistent format.
# For now, ignore responses which are irregular such as "If I was notified before Monday then Monday. Otherwise Friday."
df = df %>% 
  mutate(
    wednesday_question = case_when(
      grepl("^((monday)|(Monday))(.)*", wednesday_question) ~ "Monday",
      grepl("^((teusday)|(Teusday))(.)*", wednesday_question) ~ "Teusday",
      grepl("^((wednesday)|(Wednesday))(.)*", wednesday_question) ~ "Wednesday",
      grepl("^((thursday)|(Thursday))(.)*", wednesday_question) ~ "Thursday",
      grepl("^((friday)|(Friday))(.)*", wednesday_question) ~ "Friday",
      grepl("^((saturday)|(Saturday))(.)*", wednesday_question) ~ "Saturday",
      grepl("^((sunday)|(Sunday))(.)*", wednesday_question) ~ "Sunday",
      TRUE ~ wednesday_question
    )
  )

# Create a new column "wednesday_question_cleaned" which when a response occurs less than 5 time, it is classified as "Other"
df = df %>%
  mutate(wednesday_question_cleaned = fct_lump_min(wednesday_question, 5))

# Convert "in_au" into logical column
df = df %>%
  mutate(
    in_au = case_when(
      grepl("^No$", in_au) ~ "FALSE",
      grepl("^Yes$", in_au) ~ "TRUE",
      TRUE ~ in_au
    )
  )
df$in_au = as.logical(df$in_au)

# Clean "fav_social_media" column
# NB: "Weixin" has been classified as "WeChat"
df = df %>%
  mutate(fav_social_media = toupper(fav_social_media)) %>%
  mutate(
    fav_social_media = case_when(
      grepl("^((INSTAGRAM)|(IG)|(INSTA)|(INSGRAM))\\s*$", fav_social_media ) ~ "Instagram",
      grepl("^((FACEBOOK)|(FB))\\s*$", fav_social_media ) ~ "Facebook",
      grepl("^TWITTER\\s*$", fav_social_media ) ~ "Twitter",
      grepl("^NONE\\s*$", fav_social_media ) ~ "None",
      grepl("^((WECHAT)|(WEIXIN)|(WETCHAT)|(WECHAT IN CHINA))\\s*$", fav_social_media ) ~ "WeChat",
      grepl("^BILIBILI\\s*$", fav_social_media ) ~ "Bilibili",
      grepl("^YOUTUBE\\s*$", fav_social_media ) ~ "Youtube",
      grepl("^REDDIT\\s*$", fav_social_media ) ~ "Reddit",
      grepl("^((TIK TOK)|(TIKTOK))\\s*$", fav_social_media ) ~ "TikTok",
      grepl("^DISCORD\\s*$", fav_social_media ) ~ "Discord",
      grepl("^MESSENGER\\s*$", fav_social_media ) ~ "Messenger",
      grepl("^IDK\\s*$", fav_social_media ) ~ "",
      grepl("^QQ\\s*$", fav_social_media ) ~ "Tencent QQ",
      grepl("^ED\\s*$", fav_social_media ) ~ "Ed Stem",
      grepl("^WEIBO\\s*$", fav_social_media ) ~ "Sina Weibo",
      grepl("^SNAPCHAT\\s*$", fav_social_media ) ~ "Snapchat",
      grepl("^BILIBILI AND YOUTUBE$", fav_social_media) ~ "Bilibili and Youtube",
      grepl("^WECHAT FACEBOOK$", fav_social_media) ~ "WeChat and Facebook",
      grepl("^INSTAGRAM/SNAP$", fav_social_media) ~ "Instagram and Snapchat",
      TRUE ~ fav_social_media
    )
  )

# Add a new column named "fav_social_media_reduced" where social media platforms which occur less than 5 times are classified as "Other"
df = df %>%
  mutate(fav_social_media_reduced = fct_lump_min(fav_social_media, 5))

# Clean "gender" column
df = df %>%
  mutate(gender = toupper(gender)) %>% 
  mutate(
    gender = case_when(
      grepl("^((MALE)|(MAN)|(M))", gender) ~ "Male",
      grepl("^((FEMALE)|(WOMAN)|(F))", gender) ~ "Female",
      grepl("^NON-BINARY", gender) ~ "Non-binary",
      TRUE ~ gender
    )
  )

# Clean "email_sign_off" column
# NB: Entries that were incredibly similar were categorised together.
#     e.g. Kindest Regards, Any Adverb + Regards, Kind Regards, Regards were all categorised as Regards.
df = df %>%
  mutate(email_sign_off = toupper(email_sign_off)) %>%
  mutate(
    email_sign_off = case_when(
      grepl("(REGARD)|(REAGRD)", email_sign_off) ~ "Regards",
      grepl("SINCERELY", email_sign_off) ~ "Sincerely",
      grepl("THANK", email_sign_off) ~ "Thanks",
      grepl("CHEERS", email_sign_off) ~ "Cheers",
      grepl("FROM", email_sign_off) ~ "From",
      grepl("HAVE A NICE DAY!", email_sign_off) ~ "Have a nice day!",
      grepl("WISH", email_sign_off) ~ "Best wishes",
      grepl("BEST", email_sign_off) ~ "Best",
      grepl("TAKE CARE", email_sign_off) ~ "Take care",
      grepl("(NOTHING)|(NONE)", email_sign_off) ~ "Nothing",
      grepl("(DEPENDS)|(VARY)", email_sign_off) ~ "Depends",
      grepl("RESPECTFULLY YOURS", email_sign_off) ~ "Respectfully yours",
      grepl("LOOKING FORWARDS TO YOUR REPLY", email_sign_off) ~ "Looking forwards to your reply",
      grepl("GOOD", email_sign_off) ~ "Good",
      grepl("YOURS", email_sign_off) ~ "Yours",
      grepl("(IS THAT RIGHT AREA)|(DEAR)", email_sign_off) ~ "",
      TRUE ~ email_sign_off
    )
  )

# Add a new column named "email_sign_off_reduced" where email sign-offs which occur less than 5 times are classified as "Other"
df = df %>%
  mutate(email_sign_off_reduced = fct_lump_min(email_sign_off, 5))

# Clean "data_scientist_salary" column.
for (i in 1:length(df$data_scientist_salary)) {
  
  current_entry = df$data_scientist_salary[i]
  current_entry = str_replace_all(current_entry, ",|\\$| |\\?", "")
  current_entry = tolower(current_entry)
  
  # Case when user inputs a dollar value for the yearly salary.
  if (grepl("^[0-9]+$", current_entry)) {
    df$data_scientist_salary[i] = current_entry
    
    # Case when user inputs yearly salary in terms of thousands of dollars. e.g. 50k
  } else if (grepl("(^([0-9]+k)$)|(^([0-9]+k/year)$)", current_entry)) {
    current_entry = str_replace_all(current_entry, "[a-z]|[:punct:]", "")
    current_entry = as.numeric(current_entry)
    df$data_scientist_salary[i] = current_entry * 1000
    
    # Case where user inputs some other way of indicating that the quoted value is a yearly rate
  } else if (grepl("(peryear)|(/year)|(p\\.a\\.)|(p\\.a)|(^[0-9]+auds$)|(^[0-9]+aud$)", current_entry)) {
    current_entry = str_replace_all(current_entry, "[a-z]|[:punct:]", "")
    df$data_scientist_salary[i] = current_entry
    
    # Manually adjust some more unique values
  } else if (current_entry == "mid60k") {
    df$data_scientist_salary[i] = "65000"
  } else if (current_entry == "50-60kannually") {
    df$data_scientist_salary[i] = "55000"
  } else if (current_entry == "2000au-4000au/month") {
    df$data_scientist_salary[i] = as.character(3000 * 12)
  } else if (current_entry == "1kpmprobs...") {
    df$data_scientist_salary[i] = 12000
  } else if (current_entry == "roughly60000aud") {
    df$data_scientist_salary[i] = 60000
    
    # Case where user inputs some way of indicated that the quoted value is a monthly rate
  } else if (grepl("month", current_entry)) {
    current_entry = str_replace_all(current_entry, "[a-z]|[:punct:]", "")
    df$data_scientist_salary[i] = as.numeric(current_entry) * 12
    
    # Case where user inputs some way of indicated that the quoted value is a weekly rate
  } else if (grepl("(pw)|(week)", current_entry)) {
    current_entry = str_replace_all(current_entry, "[a-z]|[:punct:]", "")
    df$data_scientist_salary[i] = as.numeric(current_entry) * 52
    
    # Case where user inputs some way of indicated that the quoted value is a weekly rate
    # The yearly rate was calculated based off the fact that the normal working week is 8 hours a day, 5 days a week.
  } else if (grepl("(perhour)|(/h)|(/hour)|(/hr)", current_entry)) {
    current_entry = str_replace_all(current_entry, "[a-z]|[:punct:]", "")
    df$data_scientist_salary[i] = as.numeric(current_entry) * 8 * 5 * 52
    
  } else {
    df$data_scientist_salary[i] = ""
  }
  
}

# Convert "data_scientist_salary" into numeric column
df$data_scientist_salary = as.numeric(df$data_scientist_salary)

# Convert "in_advanced" into logical column
df = df %>%
  mutate(
    in_advanced = case_when(
      in_advanced == "DATA2002"~ "FALSE",
      in_advanced == "DATA2902 (Advanced)" ~ "TRUE",
      TRUE ~ in_advanced
    )
  )
df$in_advanced = as.logical(df$in_advanced)

# Clean "uni_year" column so that responses are not as long.
df = df %>%
  mutate(
    uni_year = case_when(
      grepl("First", uni_year) ~ "First Year",
      grepl("Second", uni_year) ~ "Second Year",
      grepl("Third", uni_year) ~ "Third Year",
      TRUE ~ uni_year
    )
  )

# Re-arranged column positioning so that like columns are near each other.
df = df[,c(1:6,7,27,8:15,28,16:22,29,23:26)]


numeric_ordinal_var = c("Math ability" = "math_ability",
                        "R ability" = "r_ability",
                        "Stress level" = "stress_level",
                        "Loneliness level" = "loneliness_level")

categorical_vars = c("Gender" = "gender",
                     "Living arrangments" = "living_arrangments",
                     "Wednesday Question" = "wednesday_question_cleaned",
                     "In Australia" = "in_au",
                     "Finding DATA2x02" = "finding_data2x02",
                     "Current university year" = "uni_year",
                     "Zoom webcam on during tutorials" = "zoom_webcam",
                     "Vaccination Status" = "vaccination",
                     "Favourite scoial media platform" = "fav_social_media_reduced",
                     "Preferred email sign off" = "email_sign_off_reduced",
                     "Steak Preference" = "steak_preference")

discrete_continuous_vars = c("Number of COVID tests" = "covid_tests",
                             "Non-Spam emails received last Friday" = "emails") 

continuous_vars = c("Student height" = "height",
                    "Exercising hours for a week" = "exercising_hours",
                    "Data Scientists Salary" = "data_scientist_salary")

box_plottable = c("Student height" = "height",
                  "Exercising hours for a week" = "exercising_hours",
                  "Data Scientists Salary" = "data_scientist_salary",
                  "Math ability" = "math_ability",
                  "R ability" = "r_ability",
                  "Stress level" = "stress_level",
                  "Loneliness level" = "loneliness_level",
                  "Number of COVID tests" = "covid_tests",
                  "Non-Spam emails received last Friday" = "emails")

df_for_table = df[c(1,4:6,8:15,17:23,25:29)]

# True/false formatter from <https://clarewest.github.io/blog/post/making-tables-shiny/>
true_false_formatter <-
    formatter("span",
              style = x ~ formattable::style(
                  font.weight = "bold",
                  color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
              ))

ui <- dashboardPage(
    
    skin = "yellow",
    
    dashboardHeader(title = "Survey Analysis"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Data Table", tabName = "data_table", icon = icon("th")),
            menuItem("Visualisations", tabName = "visualisations", icon = icon("image")),
            menuItem("Goodness of Fit Test", tabName = "goodness_of_fit_test", icon = icon("search")),
            menuItem("T-Tests", tabName = "t_tests", icon = icon("search"))
        )
    ),
    
    dashboardBody(
        
        tabItems(
            
            # Home
            tabItem(tabName = "home",
                    box(
                        title = "Home",
                        status = "primary",
                        solidHeader = TRUE,
                        width = NULL,
                        p("Welcome to the", strong("DATA2902 shiny"), "which allows for the quick and easy analysis of the", strong("DATA2x02 survey data!")),
                        br(),
                        p("This shiny was developed to offer you - the viewer - the ability to explore data of your own choosing. There are so many variables here to explore, and I'm sure you'll find something that you'd like to investigate."),
                        br(),
                        p("Everything in this shiny was created using ", strong("R"), " and it's ", strong("packages"), ". For more information about the packages that were used, please keep reading on!")
                    ),
                    box(
                        title = "Packages Used",
                        status = "primary",
                        solidHeader = TRUE,
                        width = NULL,
                        h5("Tidyverse"),
                        p("The", strong("tidyverse"), "package was heavily utilised as part of the data cleaning phase. In particular, the pipe '%>%' operator, the mutate function and the case_when functions were heavily utilised. Further, the ", strong("ggplot2"), "package which is a part of the tidyverse was utilised to create the plots and visualisations evident throughout this shiny."),
                        br(),
                        h5("Plotly"),
                        p("The ", strong("plotly"), " packages was used to create the interactive visualisations."),
                        br(),
                        h5("Shiny"),
                        p("The ", strong("shiny"), " package was used as the foundation for this interactive web app."),
                        br(),
                        h5("Shinydashboard"),
                        p("The ", strong("shinydashboard"), " package was used to make the dashboard evident to the left of the screen."),
                        br(),
                        h5("DT"),
                        p("The ", strong("DT"), " package was used to present the interactive data table found on the \"Data Table\" page. DT is an R package which provides an interface to JavaScript's \"DataTables\" library. More information can be found ", a("here.", href = "https://rstudio.github.io/DT/")),
                        br(),
                        h5("Formattable"),
                        p("The ", strong("formattable"), " package was used to highlight cells in the data table created by the DT package. Cells were highlighted based upon range (for numerical values), or value (for True/False/Na values) to make it easier for the user to view the trends in the data."),
                    ),
                    box(
                        title = "Author",
                        status = "primary",
                        solidHeader = TRUE,
                        width = NULL,
                        p("This shiny was made by", strong("Thomas Elton"), "as part of an assignment for DATA2902 - Data Analytics: Learning from Data (Semester 2, 2021) from the University of Sydney.")
                    )
            ),
            
            # Data Table
            tabItem(tabName = "data_table", style = "height:1100px; overflow-y: scroll;",
                    h1("Data Table*"),
                    br(),
                    formattable(
                        df_for_table,
                        list(
                            `in_au` = true_false_formatter,
                            `in_advanced` = true_false_formatter,
                            `covid_tests` = color_tile("white", "red"),
                            `height` = color_tile("white", "red"),
                            `math_ability` = color_tile("white", "red"),
                            `r_ability` = color_tile("white", "red"),
                            `stress_level` = color_tile("white", "red"),
                            `loneliness_level` = color_tile("white", "red"),
                            `data_scientist_salary` = color_tile("white", "red"),
                            `exercising_hours` = color_tile("white", "red"),
                            `emails` = color_tile("white", "red")
                        )
                    ) %>%
                        as.datatable(
                            options = list(
                                columnDefs = list(list(className = 'dt-center', targets = "_all"))
                            ),
                            fillContainer = FALSE,
                            filter = "top"
                        ),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    p("*Numeric scales are colour-coded based upon how a parituclar response fits within the range of all responses from the sample. A response highlighted ", strong("red"), "indicates that this response was close to the maximum value recorded, and a response highlighted ", strong("white"), "indicates that this repsonse was close to the mimimum value recorded. In between responses appears as", strong("lighter shades"), "of red.", style = "font-size:15px;"),
                    br(),
                    p("*The author acknowledges that a function to colour-code TRUE and FALSE values was taken from Clare E. West (2020), and the blog where Clare provided the function can be found ", a("here.", href="https://clarewest.github.io/blog/post/making-tables-shiny/"), style = "font-size:15px;" )
                    
            ),
            
            tabItem(tabName = "visualisations",
                    # Boxes need to be put in a row (or column)
                    fluidRow( 
                        
                        # Visualisations input
                        box(
                            width = 3,
                            title = "Controls",
                            
                            # Select graph
                            selectInput(
                                inputId = "graph_type",
                                label = "Choose Plot:",
                                choices = c("Boxplot" = "boxplot",
                                            "Comparative Boxplot" = "comparative_boxplot",
                                            "Barplot" = "bar_plot",
                                            "Scatterplot" = "scatterplot"
                                )
                            ),
                            
                            # Boxplot
                            conditionalPanel(
                                condition = "input.graph_type == 'boxplot'",
                                selectInput(
                                    inputId = "boxplot_var",
                                    label = "Choose Numeric Variable:",
                                    choices = box_plottable
                                )
                            ),
                            
                            # Comparative Boxplot
                            conditionalPanel(
                                condition = "input.graph_type == 'comparative_boxplot'",
                                selectInput(
                                    inputId = "comparative_boxplot_var",
                                    label = "Choose Numeric Variable:",
                                    choices = box_plottable
                                ),
                                selectInput(
                                    inputId = "comparative_boxplot_category",
                                    label = "Choose Comparative Variable:",
                                    choices = c(
                                        "Gender" = "gender",
                                        "In Australia" = "in_au" ,
                                        "Wednesday Question" = "wednesday_question_cleaned",
                                        "Finding DATA2x02" ="finding_data2x02" ,
                                        "Current university year" = "uni_year",
                                        "Zoom webcam on during tutorials" = "zoom_webcam",
                                        "Vaccination status" = "vaccination",
                                        "Steak preference" = "steak_preference"
                                    )
                                )
                            ),
                            
                            # barplot
                            conditionalPanel(
                                condition = "input.graph_type == 'bar_plot'",
                                selectInput(
                                    inputId = "bar_plot_var",
                                    label = "Choose Categorical Variable:",
                                    choices =  categorical_vars
                                )
                            ),
                            
                            # Scatterplot
                            conditionalPanel(
                                condition = "input.graph_type == 'scatterplot'",
                                selectInput(
                                    inputId = "scatterplot_x_var",
                                    label = "Choose X-axis Variable:",
                                    choices = box_plottable
                                ),
                                selectInput(
                                    inputId = "scatterplot_y_var",
                                    label = "Choose Y-axis Variable:",
                                    choices = box_plottable
                                ),
                                selectInput(
                                    inputId = "scatterplot_categorical_var",
                                    label = "Choose Categorical Variable:",
                                    choices = categorical_vars
                                ),
                                checkboxInput(
                                    inputId = "scatterplot_regression_line",
                                    label = "Add regression lines?",
                                    value = FALSE
                                )
                                
                            )
                            
                            
                        ),
                        
                        box(
                            width = 9,
                            title = "Visualisation",
                            conditionalPanel(
                                condition = "input.graph_type == 'boxplot'",
                                plotly::plotlyOutput("boxplot")
                            ),
                            conditionalPanel(
                                condition = "input.graph_type == 'comparative_boxplot'",
                                plotly::plotlyOutput("comparative_boxplot")
                            ),
                            conditionalPanel(
                                condition = "input.graph_type == 'bar_plot'",
                                plotly::plotlyOutput("bar_plot_plotly")
                            ),
                            conditionalPanel(
                                condition = "input.graph_type == 'scatterplot'",
                                plotly::plotlyOutput("scatter_plot_plotly")
                            )
                        )
                        
                    )
            ),
            
            tabItem(tabName = "goodness_of_fit_test",
                    fluidRow(
                        
                        box(
                            width = 3,
                            title = "Controls",
                            selectInput(
                                inputId = "distibution_choice",
                                label = "Choose distribution under H0:",
                                choices = c(
                                    "Uniform" = "uniform",
                                    "Poisson" = "poisson"
                                )
                            ),
                            
                            conditionalPanel(
                                condition = "input.distibution_choice == 'uniform'",
                                selectInput(
                                    inputId = "goodness_of_fit_variable",
                                    label = "Choose variable:",
                                    choice = categorical_vars
                                )
                            ),
                            
                            conditionalPanel(
                                condition = "input.distibution_choice == 'poisson'",
                                selectInput(
                                    inputId = "poisson_variable",
                                    label = "Choose variable:",
                                    choice = c(
                                        "Covid Tests" = "covid_tests",
                                        "Non-spam Emails in 1 Day" = "emails"
                                    )
                                )
                            )
                            
                        ),
                        
                        column(
                            width = 9,
                            
                            conditionalPanel(
                                condition = "input.distibution_choice == 'uniform'",
                                box(
                                    title = "Hypothesis",
                                    status = "primary",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    p("H0 (null hypothesis): the data follows a uniform distribution."),
                                    p("vs"),
                                    p("H1 (alternate hypothesis): the data does not follow a uniform distribution.")
                                ),
                                box(
                                    title = "Expected Values Assumption",
                                    status = "success",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("The expected values assumption ", strong("is satisfied"), " as all expected frequencies are >= 5.")
                                ),
                                box(
                                    title = "Independence Assumption",
                                    status = "success",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("The independence assumption ", strong("is satisfied") ," under the data collection methodology.")
                                ),
                                box(
                                    title = "Observed Test Statistic",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('degrees_of_freedom_goodness_of_fit'),
                                    uiOutput('t_value_goodness_of_fit')
                                ),
                                box(
                                    title = "P-value",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('p_value_goodness_of_fit'),
                                    plotOutput("p_value_plot_goodness_of_fit"),
                                    br(),
                                    p("*The code to generate the graph is from:"),
                                    p("Tarr, G (2021).", em("DATA2002 Data Analytics: Learning from Data"), ". University of Sydney, Sydney Australia.")
                                ),
                                box(
                                    title = "Decision",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('descision_goodness_of_fit')
                                )
                            ),
                            
                            conditionalPanel(
                                condition = "input.distibution_choice == 'poisson'",
                                box(
                                    title = "Hypothesis",
                                    status = "primary",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    p("H0 (null hypothesis): the data follows a poisson distribution."),
                                    p("vs"),
                                    p("H1 (alternate hypothesis): the data does not follow a poisson distribution.")
                                ),
                                box(
                                    title = "Estimating λ by the Sample Mean",
                                    status = "primary",
                                    width = NULL,
                                    solidHeader = TRUE,
                                    p("Since λ is unknown, we estimate λ by the sample mean:"),
                                    uiOutput('lambda_output'),
                                    br(),
                                    p("Since we have estimated a parameter, we will lose a further degree of freedom.")
                                ),                               
                                box(
                                    title = "Expected Values Assumption",
                                    status = "danger",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("The expected values assumption", strong(" is not satisfied"), "as not all expected frequencies are >= 5.")
                                ),
                                box(
                                    title = "Independence Assumption",
                                    status = "success",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("The independence assumption ", strong("is satisfied"), " under the data collection methodology.")
                                ),
                                box(
                                    title = "Observed Test Statistic",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('degrees_of_freedom_poisson'),
                                    uiOutput('poisson_test_statistic')
                                ),
                                box(
                                    title = "P-value",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('p_value_poisson'),
                                    br(),
                                    uiOutput('p_value_poisson_plot')
                                ),
                                box(
                                    title = "Decision",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('descision_poisson')
                                )
                            )
                        )
                        
                        
                        
                    )
            ),
            
            tabItem(tabName = "t_tests",
                    
                    fluidRow(
                        
                        box(
                            width = 3,
                            title = "Controls",
                            selectInput(
                                inputId = "t_test_choice",
                                label = "Type of t-test:",
                                choices = c(
                                    "One sample t-test " = "one_sample",
                                    "Two sample t-test" = "two_sample"
                                )
                            ),
                            
                            
                            conditionalPanel(
                                condition = "input.t_test_choice == 'one_sample'",
                                selectInput(
                                    inputId = "one_sample_variable",
                                    label = "Quantitative Variable:",
                                    choice = box_plottable
                                ),
                                selectInput(
                                    inputId = "normality_visualisation",
                                    label = "Choose Visualisation To Check Normality Assumption:",
                                    choice = c(
                                        "Boxplot (Looking For Symmetry)" = "boxplot",
                                        "QQ plot (Looking For a Straight Line)" = "qq_plot"
                                    )
                                ),
                                selectInput(
                                    inputId = "one_sided_alternative",
                                    label = "Alternative Hypothesis (H1):",
                                    choice = c(
                                        "Two-sided alternative" = "two_sided",
                                        "Upper-side alternative" = "upper_sided",
                                        "Lower-sde alternative" = "lower_sided"
                                    )
                                ),
                                numericInput(
                                    inputId = "one_sided_mu",
                                    label = "Input the Value of mu for the Null Hypothesis (H0):",
                                    value = 0
                                )
                            ),
                            
                            conditionalPanel(
                                condition = "input.t_test_choice == 'two_sample'",
                                selectInput(
                                  inputId = "two_sample_category",
                                  label = "Categoric Variable to Split Data Into Two Samples:",
                                  choice = c("Gender" = "gender",
                                            "Living arrangments" = "living_arrangments",
                                            "Wednesday Question" = "wednesday_question_cleaned",
                                            "In Australia" = "in_au",
                                            "Finding DATA2x02" = "finding_data2x02",
                                            "Current university year" = "uni_year",
                                            "Zoom webcam on during tutorials" = "zoom_webcam",
                                            "Vaccination Status" = "vaccination",
                                            "Favourite scoial media platform" = "fav_social_media_reduced",
                                            "Preferred email sign off" = "email_sign_off_reduced",
                                            "Steak preference" = "steak_preference")
                                ),
                                selectInput(
                                  inputId = "sample_1",
                                  label = "Sample 1 From Categoric Variable:",
                                  choice = ""
                                ),
                                selectInput(
                                  inputId = "sample_2",
                                  label = "Sample 2 From Categoric Variable:",
                                  choice = ""
                                ),
                                selectInput(
                                  inputId = "quantitative_variable_two_sided",
                                  label = "Quantitative Variables:",
                                  choice = box_plottable
                                ),
                                selectInput(
                                  inputId = "normality_visualisation_two_sided",
                                  label = "Choose Visualisation To Check Normality Assumption:",
                                  choice = c(
                                    "Boxplot (Looking For Symmetry)" = "boxplot",
                                    "QQ plot (Looking For a Straight Line)" = "qq_plot"
                                  )
                                ),
                                selectInput(
                                  inputId = "two_sided_alternative",
                                  label = "Alternative Hypothesis (H1):",
                                  choice = c(
                                    "Two-sided alternative" = "two_sided",
                                    "Upper-side alternative" = "upper_sided",
                                    "Lower-sde alternative" = "lower_sided"
                                  )
                                )
                            )
                            
                        ),
                        
                        column(
                            width = 9,
                            conditionalPanel(
                                condition = "input.t_test_choice == 'one_sample'",
                                box(
                                    title = "Hypotheses",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('h0_one_sided'),
                                    sprintf("vs"),
                                    uiOutput("h1_one_sided")
                                ),
                                box(
                                  title = "Each Observation is Chosen at Random From a Population",
                                  status = "danger",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  p("This assumption is ", strong("not satisfied"), " under the data collection methodology.")
                                ),
                                box(
                                    title = "The Sample is Independently and Identically Distributed",
                                    status = "success",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("This assumption is assumed ", strong("satisfied"), " under the data collection methodology.")
                                ),
                                conditionalPanel(
                                    condition = "input.normality_visualisation == 'boxplot'",
                                    box(
                                        title = "Boxplot to Check Normality Assumption",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = NULL,
                                        p("To test for normality in a boxplot, we are looking for symmetry in the boxplot."),
                                        plotly::plotlyOutput("one_sided_barplot")
                                    )
                                ),
                                conditionalPanel(
                                    condition = "input.normality_visualisation == 'qq_plot'",
                                    box(
                                        title = "Boxplot to Check Normality Assumption",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = NULL,
                                        p("To test for normality in a QQ plot, we are looking for points that lie reasonably close to the line."),
                                        plotly::plotlyOutput("one_sided_qq_plot")
                                    )
                                ),
                                conditionalPanel(
                                    condition = "output.normality_assumption_one_sided === true",
                                    box(
                                        title = "Normality Assumption According to Shapiro-Wilk Test for Normality",
                                        status = "danger",
                                        solidHeader = TRUE,
                                        width = NULL,
                                        p("According to the Shapiro-Wilk test for Normality, this variable is not normally distribution, and hence this assumption ", strong("is not satisfied"), "."),
                                    )
                                ),
                                conditionalPanel(
                                    condition = "output.normality_assumption_one_sided === false",
                                    box(
                                        title = "Normality Assumption According to Shapiro-Wilk Test for Normality",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = NULL,
                                        p("According to the Shapiro-Wilk test for Normality, this variable is  normally distribution, and hence this assumption ", strong("is satisfied"), "."),
                                    )
                                ),
                                box(
                                    title = "Observed Test Statistic",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('degrees_of_freedom_one_sided'),
                                    uiOutput('test_statistic_one_sided')
                                ),
                                box(
                                    title = "P-value",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('p_value_one_sided'),
                                ),
                                box(
                                    title = "Decision",
                                    status = "primary",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    uiOutput('descision_one_sided')
                                )
                            ),
                            conditionalPanel(
                              condition = "input.t_test_choice == 'two_sample'",
                              conditionalPanel(
                                condition = "output.samples_equal === true",
                                box(
                                  title = "Error: Samples 1 and 2 are the same!",
                                  status = "danger",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  p("The option for ", strong("\"Sample 1 From Categoric Variables\""), " and ", strong("\"Sample 2 From Categoric Variables\""), " cannot be the same!")
                                )
                              ),
                              conditionalPanel(
                                condition = "output.samples_equal === false",
                                box(
                                  title = "Hypotheses",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  uiOutput('h0_two_sided_intro'),
                                  br(),
                                  sprintf("H0 (null hypothesis): μ1 = μ2"),
                                  br(),
                                  sprintf("vs"),
                                  uiOutput('h1_two_sided')
                                ),
                                box(
                                  title = "Each Observation is Chosen at Random From a Population",
                                  status = "danger",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  p("This assumption is ", strong("not satisfied"), " under the data collection methodology.")
                                ),
                                box(
                                  title = "The Samples are Independently and Identically Distributed",
                                  status = "success",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  p("This assumption is assumed ", strong("satisfied"), " under the data collection methodology.")
                                ),
                                box(
                                  title = "Sample 1 is Independent of Sample 2",
                                  status = "success",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  p("This assumption is ", strong("satisfied"), " as the samples are mutually exclusive.")
                                ),
                                conditionalPanel(
                                  condition = "input.normality_visualisation_two_sided == 'qq_plot'",
                                  box(
                                    title = "Boxplots to Check Normality Assumption",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("To test for normality in a QQ plot, we are looking for points that lie reasonably close to the line."),
                                    plotly::plotlyOutput("two_sided_qq_plot")
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.normality_visualisation_two_sided == 'boxplot'",
                                  box(
                                    title = "Boxplots to Check Normality Assumption",
                                    status = "warning",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("To test for normality in a boxplot, we are looking for symmetry in the boxplot."),
                                    plotly::plotlyOutput("two_sided_box_plot"),
                                    uiOutput('n_sample_1'),
                                    uiOutput('n_sample_2')
                                  )
                                ),
                                conditionalPanel(
                                  condition = "output.normality_assumption_two_sided === true",
                                  box(
                                    title = "Normality Assumption According to Shapiro-Wilk Test for Normality",
                                    status = "danger",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("According to the Shapiro-Wilk test for Normality, not both samples are normally distribution, and hence this assumption ", strong("is not satisfied"), ".")
                                  )
                                ),
                                conditionalPanel(
                                  condition = "output.normality_assumption_two_sided === false",
                                  box(
                                    title = "Normality Assumption According to Shapiro-Wilk Test for Normality",
                                    status = "success",
                                    solidHeader = TRUE,
                                    width = NULL,
                                    p("According to the Shapiro-Wilk test for Normality, not both samples are normally distribution, and hence this assumption ", strong("is satisfied"), ".")
                                  )
                                ),
                                box(
                                  title = "Observed Test Statistic",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  uiOutput('degrees_of_freedom_two_sided'),
                                  uiOutput('test_statistic_two_sided')
                                ),
                                box(
                                  title = "P-value",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  uiOutput('p_value_two_sided'),
                                ),
                                box(
                                  title = "Decision",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = NULL,
                                  uiOutput('descision_two_sided')
                                )
                            )
                            

                              
                            )

                        )
                        
                        
                        
                    )
                    
            )
            
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Boxplot
    x_boxplot <- reactive({
        df[,input$boxplot_var]
    })
    
    output$boxplot = plotly::renderPlotly({
        
        x_lab <- ""
        if (input$boxplot_var == "math_ability") {
            x_lab = "Math ability"
        } else if (input$boxplot_var == "r_ability") {
            x_lab = "R ability"
        } else if (input$boxplot_var == "stress_level") {
            x_lab = "Stress level"
        } else if (input$boxplot_var == "loneliness_level") {
            x_lab = "Loneliness level"
        } else if (input$boxplot_var == "covid_tests") {
            x_lab = "Number of COVID tests"
        } else if (input$boxplot_var == "emails") {
            x_lab = "Non-Spam emails received last Friday"
        } else if (input$boxplot_var == "height") {
            x_lab = "Student height"
        } else if (input$boxplot_var == "exercising_hours") {
            x_lab = "Exercising hours for a week"
        } else if (input$boxplot_var == "data_scientist_salary") {
            x_lab = "Data Scientists Salary"
        }
        
        p = df %>%
            plot_ly(type = "box",
                    y = x_boxplot(),
                    x = "",
                    name = "") %>%
            layout(
                yaxis = list(title = x_lab)#,
                #height = 500
            )
        
        return(p)
        
    })
    
    # Comparative Boxplot
    x_comparative_boxplot <- reactive({
        return(df[,input$comparative_boxplot_var])
    })
    
    variable_comparative_boxplot <- reactive({
        temp = df
        if (input$comparative_boxplot_category == "gender") {
          temp$gender[temp$gender == ""] <- "Non-Response"
        } else if (input$comparative_boxplot_category == "wednesday_question_cleaned") {
          temp$wednesday_question_cleaned[temp$wednesday_question_cleaned == ""] <- "Non-Response"
        } else if (input$comparative_boxplot_category == "vaccination") {
          temp$vaccination[temp$vaccination == ""] <- "Non-Response"
        }
        return(temp[,input$comparative_boxplot_category])
    })
    
    output$comparative_boxplot = plotly::renderPlotly({
        
        x_lab <- ""
        if (input$comparative_boxplot_var == "math_ability") {
            x_lab = "Math ability"
        } else if (input$comparative_boxplot_var == "r_ability") {
            x_lab = "R ability"
        } else if (input$comparative_boxplot_var == "stress_level") {
            x_lab = "Stress level"
        } else if (input$comparative_boxplot_var == "loneliness_level") {
            x_lab = "Loneliness level"
        } else if (input$comparative_boxplot_var == "covid_tests") {
            x_lab = "Number of COVID tests"
        } else if (input$comparative_boxplot_var == "emails") {
            x_lab = "Non-Spam emails received last Friday"
        } else if (input$comparative_boxplot_var == "height") {
            x_lab = "Student height"
        } else if (input$comparative_boxplot_var == "exercising_hours") {
            x_lab = "Exercising hours for a week"
        } else if (input$comparative_boxplot_var == "data_scientist_salary") {
            x_lab = "Data Scientists Salary"
        }
        
        y_lab <- ""
        if (input$comparative_boxplot_category == "in_au") {
            y_lab = "In Australia"
        } else if (input$comparative_boxplot_category == "wednesday_question_cleaned") {
            y_lab = "Wednesday Question"
        } else if (input$comparative_boxplot_category == "finding_data2x02") {
            y_lab = "Finding DATA2x02"
        } else if (input$comparative_boxplot_category == "uni_year") {
            y_lab = "Current university year"
        } else if (input$comparative_boxplot_category == "zoom_webcam") {
            y_lab = "Zoom webcam on during tutorials"
        } else if (input$comparative_boxplot_category == "vaccination") {
            y_lab = "Vaccination status"
        }
        
        p = df %>%
            plot_ly(type = "box",
                    y = ~x_comparative_boxplot(),
                    x = variable_comparative_boxplot(),
                    name = "") %>%
            layout(
                xaxis = list(title = y_lab),
                yaxis = list(title = x_lab),
                boxmode = 'group'#,
            )
        
        return(p)
        
    })
    
    # Bar plot
    bar_plot_data <- reactive({
        temp = df
        if (input$bar_plot_var == "gender") {
          temp$gender[temp$gender == ""] <- "Non-Response"
        } else if (input$bar_plot_var == "wednesday_question_cleaned") {
          temp$wednesday_question_cleaned[temp$wednesday_question_cleaned == ""] <- "Non-Response"
        } else if (input$bar_plot_var == "vaccination") {
          temp$vaccination[temp$vaccination == ""] <- "Non-Response"
        } else if (input$bar_plot_var == "living_arrangments") {
          temp$living_arrangments[temp$living_arrangments == ""] <- "Non-Response"
        } else if (input$bar_plot_var == "fav_social_media_reduced") {
          temp$fav_social_media_reduced[temp$fav_social_media_reduced == ""] <- "Non-Response"
        } else if (input$bar_plot_var == "email_sign_off_reduced") {
          temp$email_sign_off_reduced[temp$email_sign_off_reduced == ""] <- "Non-Response"
        } else if (input$bar_plot_var == "steak_preference") {
          temp$steak_preference[temp$steak_preference == ""] <- "Non-Response"
        }
        
        return(temp[,input$bar_plot_var])
    })
    
    label_boxplot <- reactive({
        x_label = ""
        if (input$bar_plot_var == "living_arrangments") {
            x_label = "Living arrangments"
        } else if (input$bar_plot_var == "wednesday_question_cleaned") {
            x_label = "Wednesday Question"
        } else if (input$bar_plot_var == "in_au") {
            x_label = "In Australia"
        } else if (input$bar_plot_var == "finding_data2x02") {
            x_label = "Finding DATA2x02"
        } else if (input$bar_plot_var == "uni_year") {
            x_label = "Current university year"
        } else if (input$bar_plot_var == "zoom_webcam") {
            x_label = "Zoom webcam on during tutorials"
        } else if (input$bar_plot_var == "vaccination") {
            x_label = "Vaccination Status"
        } else if (input$bar_plot_var == "fav_social_media_reduced") {
            x_label = "Favourite scoial media platform"
        } else if (input$bar_plot_var == "email_sign_off_reduced") {
            x_label = "Preferred email sign off"
        } else if (input$bar_plot_var == "gender") {
          x_label = "Gender"
        } else if (input$bar_plot_var == "steak_preference") {
          x_label = "Steak preference"
        }
        return(x_label)
    })
    
    bar_plot_ggplot = reactive({
        
        data = data.frame(c(bar_plot_data()))
        colnames(data) <- c("column")

        p = data %>% ggplot() + aes(x = column, fill = column) +
            geom_bar() +
            labs(x = label_boxplot(), y = "Count", fill = "") +
            theme_bw()
        return(p)
    })
    
    output$bar_plot_plotly = plotly::renderPlotly({
        
        font_properties <- list(size = 14)
        
        p = ggplotly(bar_plot_ggplot()) %>% 
            layout(legend = list(title=list(text=label_boxplot(),font = font_properties)), 
                   xaxis = list(tickangle=45, tickfont = list(size=10)))
        return(p)
        
    })
    
    # Scatter plot
    scatter_plot_ggplot = reactive({
        
        x_label = ""
        if (input$scatterplot_x_var == "height") {
            x_label = "Student height"
        } else if (input$scatterplot_x_var == "exercising_hours") {
            x_label = "Hours spent exercising in a week"
        } else if (input$scatterplot_x_var == "data_scientist_salary") {
            x_label = "Perceived data scientist salary"
        } else if (input$scatterplot_x_var == "math_ability") {
          x_label = "Math ability"
        } else if (input$scatterplot_x_var == "r_ability") {
          x_label = "R ability"
        } else if (input$scatterplot_x_var == "stress_level") {
          x_label = "Stress level"
        } else if (input$scatterplot_x_var == "loneliness_level") {
          x_label = "Loneliness level"
        } else if (input$scatterplot_x_var == "emails") {
          x_label = "Non-Spam emails received last Friday"
        } else if (input$scatterplot_x_var == "exercising_hours") {
          x_label = "Exercising hours"
        } else if (input$scatterplot_x_var == "covid_tests") {
          x_label = "Number of COVID tests "
        } 
        
        
        y_label = ""
        if (input$scatterplot_y_var == "height") {
          y_label = "Student height"
        } else if (input$scatterplot_y_var == "exercising_hours") {
          y_label = "Hours spent exercising in a week"
        } else if (input$scatterplot_y_var == "data_scientist_salary") {
          y_label = "Perceived data scientist salary"
        } else if (input$scatterplot_y_var == "math_ability") {
          y_label = "Math ability"
        } else if (input$scatterplot_y_var == "r_ability") {
          y_label = "R ability"
        } else if (input$scatterplot_y_var == "stress_level") {
          y_label = "Stress level"
        } else if (input$scatterplot_y_var == "loneliness_level") {
          y_label = "Loneliness level"
        } else if (input$scatterplot_y_var == "emails") {
          y_label = "Non-Spam emails received last Friday"
        } else if (input$scatterplot_y_var == "exercising_hours") {
          y_label = "Exercising hours"
        } else if (input$scatterplot_y_var == "covid_tests") {
          y_label = "Number of COVID tests "
        } 
        
        new = df
        if (input$scatterplot_categorical_var == "gender") {
          new$gender[new$gender == ""] <- "Non-Response"
        } else if (input$scatterplot_categorical_var == "wednesday_question_cleaned") {
          new$wednesday_question_cleaned[new$wednesday_question_cleaned == ""] <- "Non-Response"
        } else if (input$scatterplot_categorical_var == "vaccination") {
          new$vaccination[new$vaccination == ""] <- "Non-Response"
        } else if (input$scatterplot_categorical_var == "living_arrangments") {
          new$living_arrangments[new$living_arrangments == ""] <- "Non-Response"
        } else if (input$scatterplot_categorical_var == "fav_social_media_reduced") {
          new$fav_social_media_reduced[new$fav_social_media_reduced == ""] <- "Non-Response"
        } else if (input$scatterplot_categorical_var == "email_sign_off_reduced") {
          new$email_sign_off_reduced[new$email_sign_off_reduced == ""] <- "Non-Response"
        } else if (input$scatterplot_categorical_var == "steak_preference") {
          new$steak_preference[new$steak_preference == ""] <- "Non-Response"
        }
        
        p = new %>%
            ggplot() +
            aes(x = !! sym(input$scatterplot_x_var),
                y = !! sym(input$scatterplot_y_var),
                colour = !! sym(input$scatterplot_categorical_var)) +
            geom_point() +
            labs(x = x_label, y = y_label, colour = "") +
            theme_bw()
        
        if(input$scatterplot_regression_line){
            p = p + geom_smooth(method = "lm", se = FALSE)
        }
        
        return(p)
        
    })
    
    output$scatter_plot_plotly = plotly::renderPlotly({
        
        font_properties <- list(size = 14)
        
        category_label = ""
        if (input$scatterplot_categorical_var == "living_arrangments") {
            category_label = "Living arrangments"
        } else if (input$scatterplot_categorical_var == "wednesday_question_cleaned") {
            category_label = "Wednesday question"
        } else if (input$scatterplot_categorical_var == "in_au") {
            category_label = "In Australia"
        } else if (input$scatterplot_categorical_var == "finding_data2x02") {
            category_label = "Finding DATA2x02"
        } else if (input$scatterplot_categorical_var == "uni_year") {
            category_label = "Current university year"
        } else if (input$scatterplot_categorical_var == "zoom_webcam") {
            category_label = "Zoom webcam on during tutorials"
        } else if (input$scatterplot_categorical_var == "vaccination") {
            category_label = "Vaccination Status"
        } else if (input$scatterplot_categorical_var == "fav_social_media_reduced") {
            category_label = "Favourite scoial media platform"
        } else if (input$scatterplot_categorical_var == "email_sign_off_reduced") {
            category_label = "Preferred email sign off"
        }
        
        p = ggplotly(scatter_plot_ggplot()) %>% layout(legend = list(title=list(text=category_label,font = font_properties)))
        
        return(p)
        
    })
    
    # Goodness of fit
    goodness_of_fit_data = reactive({
        new = df %>%
            select(input$goodness_of_fit_variable) %>%
            na.omit()
        new = new[new != ""]
        new = as.factor(new)
        return(new)
    })
    
    output$t_value_goodness_of_fit <- renderUI({
        
        data = goodness_of_fit_data()
        n = length(data)
        n_levels = nlevels(data)
        p = rep(c(1/n_levels), times = n_levels)
        y = table(data)
        x_squared = as.numeric(chisq.test(y, p = p)$statistic)
        sprintf("Test statistic: %.3f", x_squared)
        
    })
    
    output$degrees_of_freedom_goodness_of_fit <- renderUI({
        
        data = goodness_of_fit_data()
        n_levels = nlevels(data) - 1
        sprintf("Degrees of Freedom: %d", n_levels)
        
    })
    
    hisquaredplot = function(t0,df,prob_statement=NULL,tail="lower",cex = 2, upper_quantile = 0.9998){
        x=LB=UB=NA
        LB = 0
        if(!is.na(t0)){
            UB = max(qchisq(upper_quantile,df),t0)
            x <- seq(LB, UB, by = .001)
            xlims = c(LB,UB)  
        } else {
            UB = qchisq(upper_quantile,df)
            x <- seq(LB, UB, by = .001)
            xlims = c(LB,UB)  
        }
        y <- dchisq(x, df)
        ylims = NULL
        if(df<=2){
            ylims = c(0,0.5)
        }
        
        if(is.null(prob_statement)){
            if(tail=="lower"){
                prob_statement = paste("P(X \u2264 ",round(t0,3),") = ",
                                       round(pchisq(t0,df,lower.tail=TRUE),4),sep="")
            } else {
                prob_statement = paste("P(X \u2265 ",round(t0,3),") = ",
                                       round(pchisq(t0,df,lower.tail=FALSE),4),sep="")
            }
        }
        
        par(mgp=c(3,1,0), mar = c(3, 1, 4, 1), cex = cex)
        plot(x, y, type = "n", axes = FALSE, 
             main=bquote(paste("Probability density function for ",chi^2,"(",.(paste0(df)),")*",sep="")),
             xlab = NA, ylab = NA, lwd = 2, xlim = xlims, ylim=ylims,
             cex=1.4,cex.lab=1.4,cex.axis=1.4,cex.main=1.4)
        axis(side = 1, at = c(0, df, round(qchisq(0.999,df)+1,0)), 
             xlim = xlims, pos = 0 , cex.axis=1.4)
        title(xlab = "x",cex.lab=1.4,mgp=c(1.8,1,0))  
        abline(v=0)
        
        if(!is.na(t0)){
            axis(side = 1, at = round(t0,3), pos = 0, col = "red", 
                 col.axis = "red", lwd = 2, cex.axis=1.4)
            if(tail=="lower"){
                #shade area to left of obtained test statistic: 
                seqs = seq(0, t0, by = 0.001)
                cord.x <- c(0, seqs, t0)
                cord.y <- c(0, dchisq(seqs, df), 0) 
                cord.y[cord.y>0.5] = 1
                polygon(cord.x, cord.y, col= rgb(1,0,0,alpha=0.5))
                segments(round(t0,3), 0, round(t0,3), dchisq(t0, df), col = "red", lwd = 2)
            } else {
                #shade area to right of obtained test statistic: 
                upper = max(UB,t0+1)
                seqs = seq(t0, upper, by = 0.001)
                cord.x <- c(t0, seqs, upper)
                cord.y <- c(0, dchisq(seqs, df), 0) 
                cord.y[cord.y>0.5] = 1
                polygon(cord.x, cord.y, col= rgb(1,0,0,alpha=0.5))
                segments(round(t0,3), 0, round(t0,3), dchisq(t0, df), col = "red", lwd = 2)
            }
            mtext(prob_statement,side=3,col = "red",cex=1.2*cex)
        }
        lines(x, y, xlab = NA, ylab = NA, lwd = 2)
    }
    
    output$p_value_plot_goodness_of_fit <- renderPlot({
        
        data = goodness_of_fit_data()
        degrees_of_freedom = nlevels(data) - 1
        
        n = length(data)
        n_levels = nlevels(data)
        p = rep(c(1/n_levels), times = n_levels)
        y = table(data)
        x_squared = as.numeric(chisq.test(y, p = p)$statistic)
        
        return(hisquaredplot(x_squared, degrees_of_freedom, tail = 'upper'))
    })
    
    output$p_value_goodness_of_fit <- renderUI({
        
        data = goodness_of_fit_data()
        n = length(data)
        n_levels = nlevels(data)
        p = rep(c(1/n_levels), times = n_levels)
        y = table(data)
        p_value = as.numeric(chisq.test(y, p = p)$p.value)
        
        if (p_value < 0.00000000000000022) {
            sprintf("P-value < 2.2e-16")
        } else {
            sprintf("P-value = %.5f", p_value)
        }
    })
    
    output$descision_goodness_of_fit <- renderUI({
        data = goodness_of_fit_data()
        n = length(data)
        n_levels = nlevels(data)
        p = rep(c(1/n_levels), times = n_levels)
        y = table(data)
        p_value = as.numeric(chisq.test(y, p = p)$p.value)
        
        if (p_value < 0.05) {
            sprintf("As the p-value is below the 0.05 level of significance, H1 (the alternate hypothesis) is supported. i.e. There is statistical evidence to suggest that the data does not follow a uniform distribution.")
        } else {
            sprintf("As the p-value is above the 0.05 level of significance, H0 (the null hypothesis) is supported. i.e. There is statistical evidence to suggest that the data does follow a uniform distribution.")
        }
        
    })
    
    poisson_data = reactive({
        new = df %>%
            select(input$poisson_variable) %>%
            na.omit()
        new = new[new != ""]
        new = as.numeric(new)
        return(new)
    })
    
    poisson_lamda = reactive({
        data = poisson_data()
        lambda = mean(data)
        return(lambda)
    })
    
    output$lambda_output <- renderUI({
        data = poisson_data()
        lambda = poisson_lamda()
        numerator = sum(data)
        denominator = length(data) - sum(is.na(data))
        sprintf("%d/%d = %.5f", numerator, denominator, lambda)
    })
    
    output$poisson_test_statistic <- renderUI({
        
        new = poisson_data()
        lam = poisson_lamda()
        
        # Elements
        n = length(new)
        
        # Discrete counts
        counts <- vector(mode = "numeric", length = (max(new)+1) )
        for (i in 1:n) {
            val = new[i]
            counts[val+1] = counts[val+1] + 1
        }
        
        # Find poisson probabilities
        p = dpois(0:10, lambda = lam)
        
        # Redefine the 11th element P(>=10), NOT P(10)
        p[11] = 1 - sum(p[0:10])
        
        # Calculate the expected frequencies
        ey = n * p
        
        # Calculating test statistic
        t0 = sum((counts - ey)^2/ey)
        
        sprintf("Test statistic: %.3f", t0)
    })
    
    output$degrees_of_freedom_poisson <- renderUI({
        
        data = poisson_data()
        degrees_of_freedom = max(data) + 1 - 1 - 1
        sprintf("Degrees of Freedom: %d", degrees_of_freedom)
        
    })
    
    output$p_value_poisson <- renderUI({
        
        new = poisson_data()
        
        degrees_of_freedom = max(new) + 1 - 1 - 1
        
        lam = poisson_lamda()
        
        # Elements
        n = length(new)
        
        # Discrete counts
        counts <- vector(mode = "numeric", length = (max(new)+1) )
        for (i in 1:n) {
            val = new[i]
            counts[val+1] = counts[val+1] + 1
        }
        
        # Find poisson probabilities
        p = dpois(0:10, lambda = lam)
        
        # Redefine the 11th element P(>=10), NOT P(10)
        p[11] = 1 - sum(p[0:10])
        
        # Calculate the expected frequencies
        ey = n * p
        
        # Calculating test statistic
        t0 = sum((counts - ey)^2/ey)
        
        p_value = 1 - pchisq(t0, degrees_of_freedom)
        
        if (p_value < 0.00000000000000022) {
            sprintf("P-value < 2.2e-16")
        } else {
            sprintf("P-value = %.5f", p_value)
        }
    })
    
    output$p_value_poisson_plot <- renderUI({
        sprintf("With such a large test statistic, it becomes impractical and too computationally expensive to produce the graph or the area under the curve.")
    })
    
    output$descision_poisson <- renderUI({
        
        new = poisson_data()
        
        degrees_of_freedom = max(new) + 1 - 1 - 1
        
        lam = poisson_lamda()
        
        # Elements
        n = length(new)
        
        # Discrete counts
        counts <- vector(mode = "numeric", length = (max(new)+1) )
        for (i in 1:n) {
            val = new[i]
            counts[val+1] = counts[val+1] + 1
        }
        
        # Find poisson probabilities
        p = dpois(0:10, lambda = lam)
        
        # Redefine the 11th element P(>=10), NOT P(10)
        p[11] = 1 - sum(p[0:10])
        
        # Calculate the expected frequencies
        ey = n * p
        
        # Calculating test statistic
        t0 = sum((counts - ey)^2/ey)
        
        p_value = 1 - pchisq(t0, degrees_of_freedom)
        
        if (p_value < 0.05) {
            sprintf("As the p-value is below the 0.05 level of significance, H1 (the alternate hypothesis) is supported. i.e. There is statistical evidence to suggest that the data does not follow a poisson distribution.")
        } else {
            sprintf("As the p-value is above the 0.05 level of significance, H0 (the null hypothesis) is supported. i.e. There is statistical evidence to suggest that the data does follow a poisson distribution.")
        }
    })
    
    # One-sided t-test
    output$h0_one_sided <- renderUI({
        sprintf("H0 (null hypothesis): mu = %.3f", input$one_sided_mu)
    })
    
    output$h1_one_sided <- renderUI({
        
        symbol = ""
        if (input$one_sided_alternative == "two_sided" ) {
            symbol = "≠"
        } else if (input$one_sided_alternative == "upper_sided") {
            symbol = ">"
        } else {
            symbol = "<"
        }
        
        sprintf("H1 (alternate hypothesis): mu %s %.3f", symbol, input$one_sided_mu)
    })
    
    one_sided_data <- reactive({
        new = df %>%
            select(input$one_sample_variable) %>%
            na.omit()
        new = new[new != ""]
        new = as.numeric(new)
        return(new)
    })
    
    # FALSE indicates the data is not normally distributed.
    # TRUE indicates the data is normally distributed.
    output$normality_assumption_one_sided <- reactive({
        data = one_sided_data()
        p_value = shapiro.test(data)$p.value
        if (p_value < 0.05) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    })
    
    outputOptions(output, "normality_assumption_one_sided", suspendWhenHidden = FALSE)
    
    output$one_sided_barplot = plotly::renderPlotly({
        
        x_lab <- ""
        if (input$one_sample_variable == "math_ability") {
            x_lab = "Math ability"
        } else if (input$one_sample_variable == "r_ability") {
            x_lab = "R ability"
        } else if (input$one_sample_variable == "stress_level") {
            x_lab = "Stress level"
        } else if (input$one_sample_variable == "loneliness_level") {
            x_lab = "Loneliness level"
        } else if (input$one_sample_variable == "covid_tests") {
            x_lab = "Number of COVID tests"
        } else if (input$one_sample_variable == "emails") {
            x_lab = "Non-Spam emails received last Friday"
        } else if (input$one_sample_variable == "height") {
            x_lab = "Student height"
        } else if (input$one_sample_variable == "exercising_hours") {
            x_lab = "Exercising hours for a week"
        } else if (input$one_sample_variable == "data_scientist_salary") {
            x_lab = "Data Scientists Salary"
        }
        
        p = df %>%
            plot_ly(type = "box",
                    x = one_sided_data(),
                    y = "",
                    name = "") %>%
            layout(
                xaxis = list(title = x_lab)#,
            )
        
        return(p)
        
    })
    
    output$one_sided_qq_plot = plotly::renderPlotly({

        data = one_sided_data()
        df = data.frame(data)

        p <- df %>% 
            ggplot() +
            aes(sample = data) +
            stat_qq() +
            stat_qq_line() +
            xlab("Theoretical Quantiles") +
            ylab("Sample Quantiles") +
            theme_bw()

        return(ggplotly(p))

    })
    
    output$degrees_of_freedom_one_sided <- renderUI({
        
        data = one_sided_data()
        degrees_of_freedom = length(data) - 1
        sprintf("Degrees of Freedom: %d", degrees_of_freedom)
        
    })
    
    output$test_statistic_one_sided <- renderUI({
        
        data = one_sided_data()
        
        test_statistic = 0
        if (input$one_sided_alternative == "two_sided") {
            test_statistic = t.test(data, mu = input$one_sided_mu, alternative = "two.sided")$statistic
        } else if (input$one_sided_alternative == "upper_sided") {
            test_statistic = t.test(data, mu = input$one_sided_mu, alternative = "greater")$statistic
        } else if (input$one_sided_alternative == "lower_sided") {
            test_statistic = t.test(data, mu = input$one_sided_mu, alternative = "less")$statistic
        }
        
        sprintf("Test statistic: %.3f", test_statistic )
    })
    
    output$p_value_one_sided <- renderUI({
        
        data = one_sided_data()
        
        p_value = 0
        if (input$one_sided_alternative == "two_sided") {
            p_value = t.test(data, mu = input$one_sided_mu, alternative = "two.sided")$p.value
        } else if (input$one_sided_alternative == "upper_sided") {
            p_value = t.test(data, mu = input$one_sided_mu, alternative = "greater")$p.value
        } else if (input$one_sided_alternative == "lower_sided") {
            p_value = t.test(data, mu = input$one_sided_mu, alternative = "less")$p.value
        }

        if (p_value < 0.00000000000000022) {
            sprintf("P-value < 2.2e-16")
        } else {
            sprintf("P-value = %.10f", p_value)
        }
    })
    
    output$descision_one_sided <- renderUI({
        
        data = one_sided_data()
        
        p_value = 0
        if (input$one_sided_alternative == "two_sided") {
            p_value = t.test(data, mu = input$one_sided_mu, alternative = "two.sided")$p.value
        } else if (input$one_sided_alternative == "upper_sided") {
            p_value = t.test(data, mu = input$one_sided_mu, alternative = "greater")$p.value
        } else if (input$one_sided_alternative == "lower_sided") {
            p_value = t.test(data, mu = input$one_sided_mu, alternative = "less")$p.value
        }
        
        if (p_value < 0.05) {
          sprintf("As the p-value is below the 0.05 level of significance, H1 (the alternate hypothesis) is supported.")
        } else {
          sprintf("As the p-value is above the 0.05 level of significance, H0 (the null hypothesis) is supported.")
        }
    })
    
    sample_buttons = function() {
      
      new = df %>%
        select(input$two_sample_category) %>%
        na.omit()
      new = new[new != ""]
      categories = unique(new)
      categories = append(categories, "Other")
      return(categories)
    }
    
    observe({
      updateSelectInput(
                        inputId  = "sample_1",
                        label = "Sample 1 From Categoric Variable:",
                        choices = sample_buttons()
      )
    })
    
    observe({
      updateSelectInput(
        inputId  = "sample_2",
        label = "Sample 2 From Categoric Variable:",
        choices = sample_buttons()
      )
    })
    
    # TRUE indicates the samples are the same.
    # FALSE indicates the samples are different
    output$samples_equal<- reactive({
      
      if (input$sample_1 == input$sample_2) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    
    outputOptions(output, "samples_equal", suspendWhenHidden = FALSE)
    
    output$h0_two_sided_intro <- renderUI({
      sprintf("Let μ1 and μ2 be the means for %s and %s respectively.", input$sample_1, input$sample_2)
    })
    
    output$h1_two_sided <- renderUI({

      symbol = ""
      if (input$two_sided_alternative == "two_sided" ) {
        symbol = "≠"
      } else if (input$two_sided_alternative == "upper_sided") {
        symbol = ">"
      } else {
        symbol = "<"
      }
      sprintf("H1 (alternate hypothesis): μ1 %s μ2", symbol)
    })
    
    sample_one_data <- reactive({
      new = df %>%
        select(c(input$two_sample_category, input$quantitative_variable_two_sided)) %>%
        na.omit()
      
      new <- filter(new, new[1] != "")
      new <- filter(new, new[2] != "")
      
      if (input$sample_1 == "Other") {
        new <- filter(new, new[1] != input$sample_2)
      } else {
        new <- filter(new, new[1] == input$sample_1)
      }
      
      return(new[2])
    })
    
    sample_two_data <- reactive({
      new = df %>%
        select(c(input$two_sample_category, input$quantitative_variable_two_sided)) %>%
        na.omit()
      
      new <- filter(new, new[1] != "")
      new <- filter(new, new[2] != "")
      
      if (input$sample_2 == "Other") {
        new <- filter(new, new[1] != input$sample_1)
      } else {
        new <- filter(new, new[1] == input$sample_2)
      }
      
      return(new[2])
    })
    
    output$two_sided_qq_plot = plotly::renderPlotly({
      
      sample_1 = sample_one_data()
      sample_2 = sample_two_data()
      
      sample_1['sample'] = "Sample 1"
      sample_2['sample'] = "Sample 2"
      
      new = bind_rows(sample_1, sample_2)
      colnames(new) <- c("value", "samples")

      p <- new %>% 
        ggplot() +
        aes(sample = value) +
        stat_qq() +
        stat_qq_line() +
        facet_grid(cols = vars(samples)) +
        xlab("Theoretical Quantiles") +
        ylab("") +
        theme_bw()
      
      p <- ggplotly(p)

      return(p)
      
    })
    
    output$two_sided_box_plot = plotly::renderPlotly({
      
      sample_1 = sample_one_data()
      sample_2 = sample_two_data()
      
      sample_1['sample'] = "Sample 1"
      sample_2['sample'] = "Sample 2"
      
      new = bind_rows(sample_1, sample_2)
      colnames(new) <- c("value", "samples")
      print(str(new))
      
      p = new %>%
        plot_ly(type = "box",
                x = ~value,
                y = ~samples,
                name = "") %>%
        layout(
          xaxis = list(title = "y_lab"),
          yaxis = list(title = ""),
          boxmode = 'group'
        )
      
      return(p)
    
    })
    
    # FALSE indicates the data is not normally distributed.
    # TRUE indicates the data is normally distributed.
    output$normality_assumption_two_sided <- reactive({
      sample_1 = sample_one_data()
      sample_1 = as.numeric(unlist(sample_1))
      sample_2 = sample_two_data()
      sample_2 = as.numeric(unlist(sample_2))
      p_value_1 = shapiro.test(sample_1)$p.value
      p_value_2 = shapiro.test(sample_2)$p.value
      if ((p_value_1 < 0.05) & (p_value_2 < 0.05)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    
    outputOptions(output, "normality_assumption_two_sided", suspendWhenHidden = FALSE)
    
    output$n_sample_1 <- renderUI({
      sample_1 = nrow(sample_one_data())
      sprintf("n(Sample 1) = %d", sample_1)
    })
    
    output$n_sample_2 <- renderUI({
      sample_2 = nrow(sample_two_data())
      sprintf("n(Sample 2) = %d", sample_2)
    })
    
    output$degrees_of_freedom_two_sided <- renderUI({
      
      data_1 = nrow(sample_one_data())
      data_2 = nrow(sample_two_data())
      degrees_of_freedom = data_1 + data_2 - 2
      sprintf("Degrees of Freedom: %d", degrees_of_freedom)
      
    })
    
    output$test_statistic_two_sided <- renderUI({
      
      sample_one = sample_one_data()
      sample_two = sample_two_data()
      
      test_statistic = 0
      if (input$two_sided_alternative == "two_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "two.sided", var.equal = TRUE)$statistic
      } else if (input$two_sided_alternative == "upper_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "greater", var.equal = TRUE)$statistic
      } else if (input$two_sided_alternative == "lower_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "less", var.equal = TRUE)$statistic
      }
      
      sprintf("Test statistic: %.3f", test_statistic)
    })
    
    output$p_value_two_sided <- renderUI({
      
      sample_one = sample_one_data()
      sample_two = sample_two_data()
      
      test_statistic = 0
      if (input$two_sided_alternative == "two_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "two.sided", var.equal = TRUE)$p.value
      } else if (input$two_sided_alternative == "upper_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "greater", var.equal = TRUE)$p.value
      } else if (input$two_sided_alternative == "lower_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "less", var.equal = TRUE)$p.value
      }
      
      if (test_statistic < 0.00000000000000022) {
        sprintf("P-value < 2.2e-16")
      } else {
        sprintf("P-value = %.10f", test_statistic)
      }
    })
    
    output$descision_two_sided <- renderUI({
      
      sample_one = sample_one_data()
      sample_two = sample_two_data()
      
      test_statistic = 0
      if (input$two_sided_alternative == "two_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "two.sided", var.equal = TRUE)$p.value
      } else if (input$two_sided_alternative == "upper_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "greater", var.equal = TRUE)$p.value
      } else if (input$two_sided_alternative == "lower_sided") {
        test_statistic = t.test(sample_one, sample_two, alternative = "less", var.equal = TRUE)$p.value
      }
      
      if (test_statistic < 0.05) {
        sprintf("As the p-value is below the 0.05 level of significance, H1 (the alternate hypothesis) is supported.")
      } else {
        sprintf("As the p-value is above the 0.05 level of significance, H0 (the null hypothesis) is supported.")
      }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
