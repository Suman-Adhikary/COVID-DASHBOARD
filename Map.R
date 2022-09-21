library(plotly)
library(rjson)


state = c("Andaman and Nicobar Islands", "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chandigarh", "Chhattisgarh",
          "Dadra and Nagar Haveli", "Daman and Diu", "Delhi", "Goa", "Gujarat", "Haryana", "Himachal Pradesh","Jammu & Kashmir",
          "Jharkhand", "Karnataka", "Kerala", "Lakshadweep", "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya",
          "Mizoram", "Nagaland", "Odisha", "Puducherry", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", "Uttar Pradesh",
          "Uttarakhand", "West Bengal", "Ladakh")
dataset <- read.csv(file = "C://Users//suman//Desktop//My//Git//COVID DASHBOARD//Data//Confirmed_cov_state.csv")
Only_state = colSums(select(dataset, "Andaman.and.Nicobar.Islands":"Ladakh"))
url="https://gist.githubusercontent.com/jbrobst/56c13bbbf9d97d187fea01ca62ea5112/raw/e388c4cae20aa53cb5090210a42ebb9b765c0a36/india_states.geojson"
geojson <- rjson::fromJSON(file=url)

g <- list(
  fitbounds = "locations",
  visible = TRUE
)

fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=geojson,
  locations=state,
  z=Only_state,
  colorscale="Bluered_r",
  featureidkey='properties.ST_NM'
)
fig <- fig %>% layout(
  geo = g
)

fig
