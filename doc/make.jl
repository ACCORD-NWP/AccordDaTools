using Documenter


pages = [
   "jbdiagnose" => "jbdiagnose.md",
   "cv header list" => "cvheaderlist.md"
]


prettyurls = get(ENV, "CI", nothing) == "true" 

format = Documenter.HTML(prettyurls = prettyurls)   

makedocs(
    sitename = "AccordDaTools",
    format = format,
    pages = pages
)

deploydocs(
    repo = "github.com/ACCORD-NWP/AccordDaTools.git",
    devbranch = "develop",   
    devurl = "dev",
)
