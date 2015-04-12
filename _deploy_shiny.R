library(shinyapps)
dir.app <- paste0(Sys.getenv('PathGitHubRepos'),'/coursera-datascience-capstone/datascience-capstone-app')
print(dir.app)

shinyapps::deployApp(dir.app)
