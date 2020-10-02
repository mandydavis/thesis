# ORVIS discard pile to clean up my script

# use lookupFromKeys() to determine what subset of keys.list to use for ScoreItems() ?
lookupFromKeys(ItemLists[461:472], ItemInfo[1:4],14) # only ask for first 14 items from each scale

# this suggests that there are ORVIS items (e.g., ORVISO, ORVISAl, etc) in ItemInfo
ItemInfo[3000:4000, "ORVIS"]

# find ORVIS_missing ???
ORVIS_missing <- SAPAdata18aug2010thru7feb2017

# this will return the question numbers and the associated item, plus two extra columns we don't really care about.
# will also limit the return list to the first 15 items
lookupFromKeys(keys.list["ipipBFASextraversion20"], ItemInfo[1:3], n=15)

#SPI keys - this does the same as above, but will give us that info for 31 scales
lookupFromKeys(keys.list[366:397], ItemInfo[1:3], n=15)

test <- lookupItems(content="I like",dictionary=ItemInfo)
lookupItems(content="chemist",dictionary=ItemInfo[1:4])
colnames(SAPAdata18aug2010thru7feb2017[500:953])
lookupItems("q_3103", ItemInfo) # that item does not exist

# now need to find scoring key of them
# riasec = realistic, entreprising, conventional, etc ... 6D representation of interests
# use error.dots plot to show gender differences like he did in class

# itemLists
names(ItemLists)
# 451 - ORVIS, by scale
lookupFromKeys(ItemLists[461:472], ItemInfo[1:4],14) # only ask for first 14 items from each scale
# ItemInfo[1:4] to limit to columns we return (ItemInfo)
# FIRST ORVIS ITEM IS 450

# find vignette of scoreItems "Scoring Scales with Psych"
#keys come from itemlists
# score the items
# impute = "none"
# first need to do lookupfromkeys??
scoreItems(keys = keys.list[390:400], items = SAPAdata18aug2010thru7feb2017, impute = "none")

# some riasec items are not in dataset so do the ones you can
# can do ORVIS all the way through, probs can't do ORAIS w this dataset tho