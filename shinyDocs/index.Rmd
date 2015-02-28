---
title       : An app for pricing US Representatives
subtitle    : don't hire an expensive lobbyist if a competent bag man will do
author      : Gabi Huiber
job         : consultant
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : [shiny]       # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides

---

## The motivation

* [Lawmakers Who Upheld NSA Phone Spying Received Double the Defense Industry Cash](http://www.wired.com/2013/07/money-nsa-vote/).

---

## The working example

* In response to the first wave of Snowden revelations, US Representative Justin Amash (R, Mich.) introduced the "Defund the NSA" amendment on July 24, 2013.
* The amendment failed 217 No to 205 Yes, in line with the wishes of:
 - the President of the United States (on record  [here](http://www.businessinsider.com/amash-amendment-nsa-white-house-obama-veto-2013-7))
 - security and defense industry interests (the surveillance state is a customer)
 - US citizens who don't think that the NSA is doing anything wrong, and whose representatives carried the day (this is just how the system should work)

--- .class #id 

## Some questions you might ask

1. OK, so No votes collected more from defense interests than Yes votes did. So what? Do US representatives really respond to funding from special interests?
1. If it turns out that they do, could you get an idea of how much might do the job?
3. Could you use the recipe for pricing votes on other issues where interested parties might be curious?

---

## The model

* Using data available in .csv format [here](http://maplight.org/us-congress/bill/113-hr-2397/1742215/contributions-by-vote?sort=asc&order=$%20From%20Interest%20Groups%3Cbr%20/%3EThat%20Opposed&party[D]=D&party[R]=R&party[I]=I&vote[AYE]=AYE&vote[NOE]=NOE&vote[NV]=NV&voted_with[with]=with&voted_with[not-with]=not-with&state=&custom_from=01/01/2011&custom_to=12/31/2012&all_pols=1&uid=44999&interests-support=&interests-oppose=D2000-D3000-D5000-D9000-D4000-D0000-D6000&from=01-01-2011&to=12-31-2012&source=pacs-nonpacs&campaign=congressional) you can estimate the probability of a No vote as a function of party affiliation and industry funding using logistic regression.
* You can do this under two modeling assumptions: 
 - there's a baseline difference (e.g. Republicans are more likely to vote No, as their constituents might be law-and-order types who take a dim view of Snowden) but there is no difference in how representatives from either party respond to funding.
 - there's both a baseline difference and a difference in how representatives from each party respond to funding (e.g. a Democrat who gets defense funding is less likely to show proper gratitude than a Republican counterpart, at any funding level).
* If under either assumption the response is flat over a plausible range of funding, that is evidence that special interests are wasting their time: when things do go their way, which makes it look like their money is well spent, that's just a coincidence between what they want and what the constituents of the majority want (wouldn't that be nice?).  

---

## The quantity of interest

* We don't want to just run a logit under each assumption above, report the parameter estimates, and go home. These days we can and should do better.
* We want to plot the _expected probability of a No vote over a reasonable range of funding levels_, by party, and simulate the uncertainty surrounding it.
* This would be useful: it would immediately tell an interested party how much for a vote in their favor, with probabillity shown within a chosen confidence band -- 80%, 95%, etc.
* This is both easy to do now with [Zelig](http://zeligproject.org/) and has been the recommended approach since [this seminal paper](http://gking.harvard.edu/files/abs/making-abs.shtml) in 2001.

---

## The app

<iframe src=https://ghuiber.shinyapps.io/Amash/></iframe>


