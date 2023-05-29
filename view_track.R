m_1 <- amwo_hmm %>% 
  filter(ID == "RI-2021-52-2022") %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

m_2 <- m_1 %>% 
  arrange(time) %>% 
  summarise(do_union=FALSE) %>% 
  st_cast("LINESTRING")

mapview::mapview(m_1) + mapview::mapview(m_2)
