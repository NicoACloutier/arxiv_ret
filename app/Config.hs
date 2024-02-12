module Config where

-- EDIT THIS FILE to change default values.
-- Make sure all values are left inside of quotes.

-- |First argument: search field.
--  This is the field that the search will be performed in.
--  The options are as folows:
--      "ti": search within titles of articles.
--      "au": search within authors of articles.
--      "abs": search within article summaries.
--      "co": search within article comments.
--      "jr": search within journal reference.
--      "cat": search for a category.
--      "rn": search for a reference number.
--      "id": search for paper ID.
--      "all": search in all fields.
--  "cat" by default.
searchField :: String
searchField = "cat" -- EDIT HERE

-- |The search query, searched within the search field.
--  "cs.CL" by default.
--  If searching by category, below is a list of categories to choose from:
--      Physics:
--          "astro-ph": Astrophysics
--          "cond-mat": Condensed matter
--          "gr-qc": General relativity and quantum cosmology
--          "hep-ex": High energy physics - experiment
--          "hep-lat": High energy physics - lattice
--          "hep-ph": High energy physics - phenomenology
--          "hep-th": High energy physics - theory
--          "math-ph": Mathematical physics
--          "nlin": Nonlinear sciences
--          "nucl-ex": Nuclear experiment
--          "nucl-th": Nuclear theory
--          "quant-ph": Quantum physics
--      Mathematics:
--          "math.AG": Algebraic geometry
--          "math.AT": Algebraic topology
--          "math.AP": Analysis of PDEs
--          "math.CT": Category theory
--          "math.CA": Classical analysis of ODEs
--          "math.CO": Combinatorics
--          "math.AC": Commutative algebra
--          "math.CV": Complex variables
--          "math.DG": Differential geometry
--          "math.DS": Dynamical systems
--          "math.FA": Functional analysis
--          "math.GM": General mathematics
--          "math.GN": General topology
--          "math.GR": Group theory
--          "math.HO": History and overview
--          "math.IT": Information theory
--          "math.KT": K-theory and homology
--          "math.LO": Logic
--          "math.MP": Mathematical physics
--          "math.MG": Metric geometry
--          "math.NT": Number theory
--          "math.NA": Numerical analysis
--          "math.OA": Operator algebras
--          "math.OC": Optimization and control
--          "math.PR": Probability
--          "math.QA": Quantum algebra
--          "math.RT": Representation theory
--          "math.RA": Rings and algebras
--          "math.SP": Spectral theory
--          "math.ST": Statistics theory
--          "math.SG": Sympleptic geometry
--      Computer science:
--          "cs.AI": Artificial intelligence
--          "cs.CL": Computation and language
--          "cs.CC": Computational complexity
--          "cs.CE": Computer engineering, finance, and science
--          "cs.CG": Computational geometry
--          "cs.GT": Computer science and game theory
--          "cs.CV": Computer vision and pattern recognition
--          "cs.CY": Computers and society
--          "cs.CR": Cryptography and security
--          "cs.DS": Data structures and algorithms
--          "cs.DB": Databases
--          "cs.DL": Digital libraries
--          "cs.DM": Discrete mathematics
--          "cs.DC": Distributed, parallel, and cluster computing
--          "cs.ET": Emerging technologies
--          "cs.FL": Formal languages and automata theory
--          "cs.GL": General literature
--          "cs.GR": Graphics
--          "cs.AR": Hardware architecture
--          "cs.HC": Human-computer interaction
--          "cs.IR": Information retrieval
--          "cs.IT": Information theory
--          "cs.LO": Logic in computer science
--          "cs.LG": Machine learning
--          "cs.MS": Mathematical software
--          "cs.MA": Multiagent systems
--          "cs.MM": Multimedia
--          "cs.NI": Network and internet architecture
--          "cs.NE": Neural and evolutionary computing
--          "cs.NA": Numerical analysis
--          "cs.OS": Operating systems
--          "cs.OH": Other computer science
--          "cs.PF": Performance
--          "cs.PL": Programming languages
--          "cs.RO": Robotics
--          "cs.SI": Social and information networks
--          "cs.SE": Software engineering
--          "cs.SD": Sound
--          "cs.SC": Symbolic computation
--          "cs.SY": Systems and control
--      Quantitative biology:
--          "q-bio.BM": Biomolecules
--          "q-bio.CB": Cell behavior
--          "q-bio.GN": Genomics
--          "q-bio.MN": Molecular networks
--          "q-bio.NC": Neurons and cognition
--          "q-bio.OT": Other quantitative biology
--          "q-bio.PE": Populations and evolution
--          "q-bio.QM": Quantitative methods
--          "q-bio.SC": Subcellular processes
--          "q-bio.TO": Tissues and organs
--      Quantitative finance:
--          "q-fin.CP": Computational finance
--          "q-fin.EC": Economics
--          "q-fin.GN": General finance
--          "q-fin.MF": Mathematical finance
--          "q-fin.PM": Portfolio management
--          "q-fin.PR": Pricing of securities
--          "q-fin.RM": Risk management
--          "q-fin.ST": Statistical finance
--          "q-fin.TR": Trading and market microstructure
--      Statistics:
--          "stat.AP": Applications
--          "stat.CO": Computation
--          "stat.ML": Machine learning
--          "stat.ME": Methodology
--          "stat.OT": Other statistics
--          "stat.TH": Statistics theory
--      Electrical engineering and systems science:
--          "eess.AS": Audio and speech processing
--          "eess.IV": Image and video processing
--          "eess.SP": Signal processing
--          "eess.SY": Systems and control
--      Economics:
--          "econ.EM": Econometrics
--          "econ.GN": General economics
--          "econ.TH": Theoretical economics
searchQuery :: String
searchQuery = "cs.CL" -- EDIT HERE

-- |The beginning value to return (0-indexed).
--  "0" by default.
beginning :: String
beginning = "0" -- EDIT HERE

-- |The maximum number of papers to return.
--  "10" by default.
maximum :: String
maximum = "10" -- EDIT HERE

-- |The key to sort by.
--  The options are as follows:
--      "rel": relevance.
--      "submitted": last submitted date.
--      "updated": last updated date.
--  "submitted" by default.
sortKey :: String
sortKey = "submitted" -- EDIT HERE

-- |The order to sort in, either ascending or descending according to the above key.
--  The options are as follows:
--      "desc": descending order.
--      "asc": ascending order.
--  "desc" by default.
sortOrder :: String
sortOrder = "desc"