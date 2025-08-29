library(tibble)

# --- OECD Fields of Science (FOS) ---
oecd_fos <- tribble(
  ~major_field_code, ~major_field, ~minor_field_code, ~minor_field,
  # 1 Natural sciences
  "1", "Natural sciences", "1.1", "Mathematics",
  "1", "Natural sciences", "1.2", "Computer and information sciences",
  "1", "Natural sciences", "1.3", "Physical sciences",
  "1", "Natural sciences", "1.4", "Chemical sciences",
  "1", "Natural sciences", "1.5", "Earth and related environmental sciences",
  "1", "Natural sciences", "1.6", "Biological sciences",
  "1", "Natural sciences", "1.7", "Other natural sciences",
  
  # 2 Engineering and technology
  "2", "Engineering and technology", "2.1", "Civil engineering",
  "2", "Engineering and technology", "2.2", "Electrical, electronic and information engineering",
  "2", "Engineering and technology", "2.3", "Mechanical engineering",
  "2", "Engineering and technology", "2.4", "Chemical engineering",
  "2", "Engineering and technology", "2.5", "Materials engineering",
  "2", "Engineering and technology", "2.6", "Medical engineering",
  "2", "Engineering and technology", "2.7", "Environmental engineering",
  "2", "Engineering and technology", "2.8", "Environmental biotechnology",
  "2", "Engineering and technology", "2.9", "Industrial biotechnology",
  "2", "Engineering and technology", "2.10", "Nano-technology",
  "2", "Engineering and technology", "2.11", "Other engineering and technologies",
  
  # 3 Medical and health sciences
  "3", "Medical and health sciences", "3.1", "Basic medicine",
  "3", "Medical and health sciences", "3.2", "Clinical medicine",
  "3", "Medical and health sciences", "3.3", "Health sciences",
  "3", "Medical and health sciences", "3.4", "Medical biotechnology",
  "3", "Medical and health sciences", "3.5", "Other medical sciences",
  
  # 4 Agricultural sciences
  "4", "Agricultural sciences", "4.1", "Agriculture, forestry, and fisheries",
  "4", "Agricultural sciences", "4.2", "Animal and dairy science",
  "4", "Agricultural sciences", "4.3", "Veterinary science",
  "4", "Agricultural sciences", "4.4", "Agricultural biotechnology",
  "4", "Agricultural sciences", "4.5", "Other agricultural sciences",
  
  # 5 Social sciences
  "5", "Social sciences", "5.1", "Psychology",
  "5", "Social sciences", "5.2", "Economics and business",
  "5", "Social sciences", "5.3", "Educational sciences",
  "5", "Social sciences", "5.4", "Sociology",
  "5", "Social sciences", "5.5", "Law",
  "5", "Social sciences", "5.6", "Political science",
  "5", "Social sciences", "5.7", "Social and economic geography",
  "5", "Social sciences", "5.8", "Media and communications",
  "5", "Social sciences", "5.9", "Other social sciences",
  
  # 6 Humanities
  "6", "Humanities", "6.1", "History and archaeology",
  "6", "Humanities", "6.2", "Languages and literature",
  "6", "Humanities", "6.3", "Philosophy, ethics and religion",
  "6", "Humanities", "6.4", "Arts",
  "6", "Humanities", "6.5", "Other humanities"
)

write.table(oecd_fos, "data/oecd_fos.csv", sep = ";")