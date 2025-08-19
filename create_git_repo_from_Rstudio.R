install.packages("usethis")
usethis::create_github_token()
gitcreds::gitcreds_set()

# create R studio project > new directory > new project, check "create a git repository" box.

# in the terminal:
# git init
# git add .
# git commit -m "initial commit"

# git branch -M main

# in R console
usethis::use_github(private = TRUE)
