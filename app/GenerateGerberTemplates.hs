module GenerateGerberTemplates (
gerberHeader,
gerberFooter,
drawings
) where

gerberHeader :: String
gerberHeader = "\
  \G75*\n\
  \%MOMM*%\n\
  \%OFA0B0*%\n\
  \%FSLAX33Y33*%\n\
  \%IPPOS*%\n\
  \%LPD*%\n\
  \%AMOC8*\n\
  \5,1,8,0,0,1.08239X$1,22.5*\n\
  \%\n\
  \\n\
  \G04 ---- Definition of 3 tools - number 10, 11 and 12 with different widths: ---- *\n\
  \%ADD10C,1.880*%\n\
  \%ADD11C,0.508*%\n\
  \%ADD12C,2.508*%\n\
  \\n\
  \G04 ---- Starting generated output: ---- *\
  \\n\n\
\"

gerberFooter :: String
gerberFooter = "\
  \G04 ---- End of generated output. ---- *\
  \\n\
  \M02*\
\"

drawing1 :: Int -> Int -> String
drawing1 x y = "\
  \G04 <<<< DRAWING 1 >>>> *\n\
  \D11*\n\
  \X" ++ show(10000 + x) ++ "Y" ++ show(20000 + y) ++ "D03*\n\
  \X" ++ show(20000 + x) ++ "Y" ++ show(20000 + y) ++ "D01*\n\
  \X" ++ show(10000 + x) ++ "Y" ++ show(10000 + y) ++ "D01*\n\
  \\n\
\"

drawing2 :: Int -> Int -> String
drawing2 x y = "\
\G04 <<<< DRAWING 2 >>>> *\n\
\D12*\n\
\X" ++ show(20000 + x) ++ "Y" ++ show(10000 + y) ++ "D02*\n\
\X" ++ show(12000 + x) ++ "Y" ++ show(15000 + y) ++ "D03*\n\
\X" ++ show(18000 + x) ++ "Y" ++ show(15000 + y) ++ "D01*\n\
\\n\
\"

drawing3 :: Int -> Int -> String
drawing3 x y = "\
\G04 <<<< DRAWING 3 >>>> *\n\
\D10*\n\
\X" ++ show(05334 + x) ++ "Y" ++ show(06604 + y) ++ "D03*\n\
\X" ++ show(05334 + x) ++ "Y" ++ show(14224 + y) ++ "D03*\n\
\X" ++ show(23114 + x) ++ "Y" ++ show(06604 + y) ++ "D03*\n\
\D11*\n\
\X" ++ show(23114 + x) ++ "Y" ++ show(10414 + y) ++ "D01*\n\
\X" ++ show(23112 + x) ++ "Y" ++ show(10536 + y) ++ "D01*\n\
\X" ++ show(23106 + x) ++ "Y" ++ show(10658 + y) ++ "D01*\n\
\X" ++ show(23096 + x) ++ "Y" ++ show(10780 + y) ++ "D01*\n\
\X" ++ show(23083 + x) ++ "Y" ++ show(10901 + y) ++ "D01*\n\
\X" ++ show(23065 + x) ++ "Y" ++ show(11022 + y) ++ "D01*\n\
\X" ++ show(23044 + x) ++ "Y" ++ show(11142 + y) ++ "D01*\n\
\X" ++ show(23018 + x) ++ "Y" ++ show(11262 + y) ++ "D01*\n\
\X" ++ show(22989 + x) ++ "Y" ++ show(11380 + y) ++ "D01*\n\
\X" ++ show(22957 + x) ++ "Y" ++ show(11498 + y) ++ "D01*\n\
\X" ++ show(22920 + x) ++ "Y" ++ show(11615 + y) ++ "D01*\n\
\X" ++ show(22880 + x) ++ "Y" ++ show(11730 + y) ++ "D01*\n\
\X" ++ show(22836 + x) ++ "Y" ++ show(11844 + y) ++ "D01*\n\
\X" ++ show(22788 + x) ++ "Y" ++ show(11956 + y) ++ "D01*\n\
\X" ++ show(22737 + x) ++ "Y" ++ show(12067 + y) ++ "D01*\n\
\X" ++ show(22682 + x) ++ "Y" ++ show(12176 + y) ++ "D01*\n\
\X" ++ show(22624 + x) ++ "Y" ++ show(12284 + y) ++ "D01*\n\
\X" ++ show(22562 + x) ++ "Y" ++ show(12389 + y) ++ "D01*\n\
\X" ++ show(22497 + x) ++ "Y" ++ show(12492 + y) ++ "D01*\n\
\X" ++ show(22429 + x) ++ "Y" ++ show(12594 + y) ++ "D01*\n\
\X" ++ show(22357 + x) ++ "Y" ++ show(12693 + y) ++ "D01*\n\
\X" ++ show(22283 + x) ++ "Y" ++ show(12789 + y) ++ "D01*\n\
\X" ++ show(22205 + x) ++ "Y" ++ show(12884 + y) ++ "D01*\n\
\X" ++ show(22124 + x) ++ "Y" ++ show(12975 + y) ++ "D01*\n\
\X" ++ show(22041 + x) ++ "Y" ++ show(13065 + y) ++ "D01*\n\
\X" ++ show(21955 + x) ++ "Y" ++ show(13151 + y) ++ "D01*\n\
\X" ++ show(21865 + x) ++ "Y" ++ show(13234 + y) ++ "D01*\n\
\X" ++ show(21774 + x) ++ "Y" ++ show(13315 + y) ++ "D01*\n\
\X" ++ show(21679 + x) ++ "Y" ++ show(13393 + y) ++ "D01*\n\
\X" ++ show(21583 + x) ++ "Y" ++ show(13467 + y) ++ "D01*\n\
\X" ++ show(21484 + x) ++ "Y" ++ show(13539 + y) ++ "D01*\n\
\X" ++ show(21382 + x) ++ "Y" ++ show(13607 + y) ++ "D01*\n\
\X" ++ show(21279 + x) ++ "Y" ++ show(13672 + y) ++ "D01*\n\
\X" ++ show(21174 + x) ++ "Y" ++ show(13734 + y) ++ "D01*\n\
\X" ++ show(21066 + x) ++ "Y" ++ show(13792 + y) ++ "D01*\n\
\X" ++ show(20957 + x) ++ "Y" ++ show(13847 + y) ++ "D01*\n\
\X" ++ show(20846 + x) ++ "Y" ++ show(13898 + y) ++ "D01*\n\
\X" ++ show(20734 + x) ++ "Y" ++ show(13946 + y) ++ "D01*\n\
\X" ++ show(20620 + x) ++ "Y" ++ show(13990 + y) ++ "D01*\n\
\X" ++ show(20505 + x) ++ "Y" ++ show(14030 + y) ++ "D01*\n\
\X" ++ show(20388 + x) ++ "Y" ++ show(14067 + y) ++ "D01*\n\
\X" ++ show(20270 + x) ++ "Y" ++ show(14099 + y) ++ "D01*\n\
\X" ++ show(20152 + x) ++ "Y" ++ show(14128 + y) ++ "D01*\n\
\X" ++ show(20032 + x) ++ "Y" ++ show(14154 + y) ++ "D01*\n\
\X" ++ show(19912 + x) ++ "Y" ++ show(14175 + y) ++ "D01*\n\
\X" ++ show(19791 + x) ++ "Y" ++ show(14193 + y) ++ "D01*\n\
\X" ++ show(19670 + x) ++ "Y" ++ show(14206 + y) ++ "D01*\n\
\X" ++ show(19548 + x) ++ "Y" ++ show(14216 + y) ++ "D01*\n\
\X" ++ show(19426 + x) ++ "Y" ++ show(14222 + y) ++ "D01*\n\
\X" ++ show(19304 + x) ++ "Y" ++ show(14224 + y) ++ "D01*\n\
\X" ++ show(05334 + x) ++ "Y" ++ show(14224 + y) ++ "D01*\n\
\"

drawing4 :: Int -> Int -> String
drawing4 x y = "\
\G04 <<<< DRAWING 4 >>>> *\n\
\D10*\n\
\X" ++ show(06858 + x) ++ "Y" ++ show(11822 + y) ++ "D02*\n\
\X" ++ show(06858 + x) ++ "Y" ++ show(12217 + y) ++ "D01*\n\
\X" ++ show(06858 + x) ++ "Y" ++ show(11822 + y) ++ "D02*\n\
\X" ++ show(08280 + x) ++ "Y" ++ show(11822 + y) ++ "D01*\n\
\X" ++ show(07648 + x) ++ "Y" ++ show(11822 + y) ++ "D02*\n\
\X" ++ show(07648 + x) ++ "Y" ++ show(12217 + y) ++ "D01*\n\
\X" ++ show(07648 + x) ++ "Y" ++ show(12296 + y) ++ "D02*\n\
\X" ++ show(08280 + x) ++ "Y" ++ show(12613 + y) ++ "D01*\n\
\X" ++ show(08280 + x) ++ "Y" ++ show(13290 + y) ++ "D02*\n\
\X" ++ show(06858 + x) ++ "Y" ++ show(14238 + y) ++ "D01*\n\
\X" ++ show(06858 + x) ++ "Y" ++ show(13290 + y) ++ "D02*\n\
\X" ++ show(08280 + x) ++ "Y" ++ show(14238 + y) ++ "D01*\n\
\X" ++ show(09525 + x) ++ "Y" ++ show(14194 + y) ++ "D02*\n\
\X" ++ show(10947 + x) ++ "Y" ++ show(13246 + y) ++ "D01*\n\
\X" ++ show(10947 + x) ++ "Y" ++ show(14194 + y) ++ "D02*\n\
\X" ++ show(09525 + x) ++ "Y" ++ show(13246 + y) ++ "D01*\n\
\X" ++ show(09525 + x) ++ "Y" ++ show(12656 + y) ++ "D02*\n\
\X" ++ show(09525 + x) ++ "Y" ++ show(11866 + y) ++ "D01*\n\
\X" ++ show(09525 + x) ++ "Y" ++ show(12261 + y) ++ "D02*\n\
\X" ++ show(10947 + x) ++ "Y" ++ show(12261 + y) ++ "D01*\n\
\X" ++ show(12192 + x) ++ "Y" ++ show(12635 + y) ++ "D02*\n\
\X" ++ show(13614 + x) ++ "Y" ++ show(13425 + y) ++ "D01*\n\
\X" ++ show(12192 + x) ++ "Y" ++ show(13425 + y) ++ "D01*\n\
\X" ++ show(12192 + x) ++ "Y" ++ show(12635 + y) ++ "D02*\n\
\X" ++ show(13614 + x) ++ "Y" ++ show(12635 + y) ++ "D01*\n\
\D11*\n\
\X" ++ show(17433 + x) ++ "Y" ++ show(10264 + y) ++ "D02*\n\
\X" ++ show(18788 + x) ++ "Y" ++ show(10264 + y) ++ "D01*\n\
\X" ++ show(18110 + x) ++ "Y" ++ show(09587 + y) ++ "D02*\n\
\X" ++ show(18110 + x) ++ "Y" ++ show(10942 + y) ++ "D01*\n\
\X" ++ show(17433 + x) ++ "Y" ++ show(06454 + y) ++ "D02*\n\
\X" ++ show(18788 + x) ++ "Y" ++ show(06454 + y) ++ "D01*\n\
\X" ++ show(53966 + x) ++ "Y" ++ show(19634 + y) ++ "D02*\n\
\X" ++ show(53966 + x) ++ "Y" ++ show(20989 + y) ++ "D01*\n\
\X" ++ show(54023 + x) ++ "Y" ++ show(21553 + y) ++ "D02*\n\
\X" ++ show(53910 + x) ++ "Y" ++ show(21553 + y) ++ "D01*\n\
\X" ++ show(53910 + x) ++ "Y" ++ show(21666 + y) ++ "D01*\n\
\X" ++ show(54023 + x) ++ "Y" ++ show(21666 + y) ++ "D01*\n\
\X" ++ show(54023 + x) ++ "Y" ++ show(21553 + y) ++ "D01*\n\
\X" ++ show(54841 + x) ++ "Y" ++ show(20650 + y) ++ "D02*\n\
\X" ++ show(54841 + x) ++ "Y" ++ show(19973 + y) ++ "D01*\n\
\X" ++ show(54843 + x) ++ "Y" ++ show(19938 + y) ++ "D01*\n\
\X" ++ show(54848 + x) ++ "Y" ++ show(19903 + y) ++ "D01*\n\
\X" ++ show(54858 + x) ++ "Y" ++ show(19868 + y) ++ "D01*\n\
\"


drawings = [drawing1, drawing2, drawing3, drawing4]
