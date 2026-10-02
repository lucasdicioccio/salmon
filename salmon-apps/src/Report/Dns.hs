{-# LANGUAGE OverloadedStrings #-}

{- | The pure half of @salmon-report@'s DNS setup probes: what @dig@ printed,
read into records, and the judgement drawn from them. No IO here; "Report"
holds the shell-outs.

The case these exist for: a domain registered at a registrar, to be served by
a hosted zone. The zone looks perfect from the inside whatever the registrar
says, so each question is asked of whoever actually decides the answer: the
parent's servers for the delegation, the expected servers for what the zone
holds, a resolver for what the world sees.
-}
module Report.Dns (
    -- * dig output
    DigRecord (..),
    DigAnswer (..),
    parseDig,
    parseDigNames,
    normalizeName,
    nsNames,

    -- * Judgements
    Judgement (..),

    -- * Delegation
    parentCandidates,
    Delegation (..),
    classifyDelegation,
    judgeDelegation,

    -- * The zone's own servers
    ServerView (..),
    serverView,
    judgeZone,

    -- * Names
    RecordQuery (..),
    parseRecordQuery,
    answerValues,
    judgeRecord,
) where

import Data.Char (isAlpha, isDigit, toUpper)
import Data.List (nub, sort, (\\))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

-------------------------------------------------------------------------------
-- dig output

-- | One resource record line; the owner is normalised, the data is as printed.
data DigRecord = DigRecord
    { recOwner :: Text
    , recType :: Text
    , recData :: Text
    }
    deriving (Eq, Show)

{- | What a @dig +noall +comments +answer [+authority]@ printed. 'digStatus'
is 'Nothing' when there was no header at all, i.e. nothing answered.
-}
data DigAnswer = DigAnswer
    { digStatus :: Maybe Text
    , digAuthoritative :: Bool
    -- ^ the @aa@ flag: the server answers for a zone it holds
    , digRecords :: [DigRecord]
    }
    deriving (Eq, Show)

-- | A DNS name for comparison: lower-case, no trailing dot.
normalizeName :: Text -> Text
normalizeName = Text.dropWhileEnd (== '.') . Text.toLower . Text.strip

parseDig :: Text -> DigAnswer
parseDig out =
    DigAnswer
        { digStatus = case mapMaybe status ls of
            (s : _) -> Just s
            [] -> Nothing
        , digAuthoritative = any hasAa ls
        , digRecords = mapMaybe record ls
        }
  where
    ls = fmap Text.strip (Text.lines out)
    status l
        | ";; ->>HEADER<<-" `Text.isPrefixOf` l
        , (_, rest) <- Text.breakOn "status: " l
        , not (Text.null rest) =
            Just (Text.takeWhile (/= ',') (Text.drop 8 rest))
        | otherwise = Nothing
    hasAa l = case Text.stripPrefix ";; flags:" l of
        Just rest -> "aa" `elem` Text.words (Text.takeWhile (/= ';') rest)
        Nothing -> False
    record l
        | ";" `Text.isPrefixOf` l = Nothing
        | (owner : ttl : "IN" : ty : rest@(_ : _)) <- Text.words l
        , Text.all isDigit ttl =
            Just (DigRecord (normalizeName owner) (Text.toUpper ty) (Text.unwords rest))
        | otherwise = Nothing

-- | The names on the lines of a @dig +short NS@, normalised.
parseDigNames :: Text -> [Text]
parseDigNames out =
    [ normalizeName l
    | l <- Text.strip <$> Text.lines out
    , not (Text.null l)
    , not (";" `Text.isPrefixOf` l)
    , length (Text.words l) == 1
    ]

-- | The NS targets an answer holds for this owner, sorted and normalised.
nsNames :: Text -> DigAnswer -> [Text]
nsNames domain ans =
    sort . nub $
        [normalizeName (recData r) | r <- digRecords ans, recType r == "NS", recOwner r == normalizeName domain]

-------------------------------------------------------------------------------

{- | 'jOk' is 'Nothing' when the question could not be answered, as opposed
to answered "no".
-}
data Judgement = Judgement
    { jOk :: Maybe Bool
    , jEvidence :: [Text]
    , jMeaning :: Maybe Text
    }
    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Delegation

{- | The names that may be the parent zone of a domain, nearest first; the
first one that has name servers is the one whose servers hold the delegation.
The root is left out: a TLD's own delegation is not this report's subject.
-}
parentCandidates :: Text -> [Text]
parentCandidates domain = go (drop 1 (Text.splitOn "." (normalizeName domain)))
  where
    go [] = []
    go ls = Text.intercalate "." ls : go (drop 1 ls)

data Delegation
    = -- | the parent says the name does not exist
      Unregistered
    | -- | the parent answered, with no name servers for the name
      NoDelegation
    | -- | delegated, to none of the expected servers
      NotDelegated [Text]
    | -- | the two sets overlap: expected servers missing, and servers handed out that were not expected
      PartlyDelegated [Text] [Text]
    | Delegated [Text]
    deriving (Eq, Show)

-- | Compares the expected set with what a parent server answered for the domain.
classifyDelegation :: Text -> [Text] -> DigAnswer -> Delegation
classifyDelegation domain expected ans
    | digStatus ans == Just "NXDOMAIN" = Unregistered
    | null observed = NoDelegation
    | observed == want = Delegated observed
    | all (`notElem` want) observed = NotDelegated observed
    | otherwise = PartlyDelegated (want \\ observed) (observed \\ want)
  where
    observed = nsNames domain ans
    want = sort (nub (normalizeName <$> expected))

{- | The delegation finding. With no expected set ('Left', saying why) the
handed-out servers are still reported, with no verdict.
-}
judgeDelegation :: Text -> Either Text [Text] -> DigAnswer -> Judgement
judgeDelegation domain expected ans = case expected of
    Left why
        | digStatus ans == Just "NXDOMAIN" -> unregistered
        | otherwise -> Judgement Nothing ([handedOut observed | not (null observed)] <> [why]) Nothing
    Right want -> case classifyDelegation domain want ans of
        Unregistered -> unregistered
        NoDelegation ->
            Judgement (Just False) ["the parent's server answered with no name servers for " <> domain] Nothing
        NotDelegated obs ->
            Judgement
                (Just False)
                [handedOut obs, "expected: " <> list want]
                (Just "Not delegated: the registrar still hands out other name servers (its own parking ones, usually), so nothing in the zone is visible to the world. Set the expected name servers at the registrar.")
        PartlyDelegated missing extra ->
            Judgement
                (Just False)
                ( [handedOut observed]
                    <> ["expected and not handed out: " <> list missing | not (null missing)]
                    <> ["handed out and not expected: " <> list extra | not (null extra)]
                )
                (Just "Partly delegated: the sets differ, so some queries reach servers that do not hold the zone. Make the registrar's list equal to the zone's.")
        Delegated obs -> Judgement (Just True) [handedOut obs] Nothing
  where
    observed = nsNames domain ans
    handedOut xs = "the parent hands out: " <> list xs
    unregistered =
        Judgement (Just False) ["the parent's server answers NXDOMAIN for " <> domain] (Just "The parent zone does not know this domain: it is not registered, or the registration holds no name servers.")

list :: [Text] -> Text
list [] = "(none)"
list xs = Text.intercalate ", " xs

-------------------------------------------------------------------------------
-- The zone's own servers

data ServerView
    = -- | nothing came back, with the reason
      Unreachable Text
    | -- | it answered, and not as a server holding the zone (the status, or "not authoritative")
      NotServing Text
    | -- | the NS set and the SOA (primary name, serial) it answers with
      Serving [Text] (Maybe (Text, Text))
    deriving (Eq, Show)

{- | What one server says of the domain, from its answers to an NS and an SOA
query asked without recursion. A 'Left' is a command that could not run.
-}
serverView :: Text -> Either Text DigAnswer -> Either Text DigAnswer -> ServerView
serverView _ (Left why) _ = Unreachable why
serverView domain (Right ns) soaAnswer = case digStatus ns of
    Nothing -> Unreachable "no answer"
    Just "NOERROR"
        | digAuthoritative ns -> Serving (nsNames domain ns) (either (const Nothing) soa soaAnswer)
        | otherwise -> NotServing "it answers without authority for the domain"
    Just other -> NotServing ("it answers " <> other)
  where
    soa ans = case [Text.words (recData r) | r <- digRecords ans, recType r == "SOA", recOwner r == normalizeName domain] of
        ((mname : _ : serial : _) : _) -> Just (normalizeName mname, serial)
        _ -> Nothing

-- | Whether the expected servers all serve the zone, and say the same thing about it.
judgeZone :: [Text] -> [(Text, ServerView)] -> Judgement
judgeZone expected views
    | null views = Judgement Nothing ["no expected name server to ask"] Nothing
    | not (null strangers) =
        Judgement (Just False) evidence (Just "An expected server does not hold this zone. A zone that is deleted and created again is assigned new name servers: read them again, then update the seed and the registrar.")
    | not (null otherSets) =
        Judgement (Just False) evidence (Just "The servers hold a zone whose own NS records are not the expected set: the expectation is stale, or the zone's apex NS records were edited.")
    | length (nub soas) > 1 =
        Judgement (Just False) evidence (Just "The servers do not hold the same version of the zone (their SOA records differ); a change may still be spreading between them.")
    | not (null silent) = Judgement Nothing evidence Nothing
    | otherwise = Judgement (Just True) evidence Nothing
  where
    want = sort (nub (normalizeName <$> expected))
    strangers = [s | (s, NotServing _) <- views]
    silent = [s | (s, Unreachable _) <- views]
    otherSets = [s | (s, Serving ns _) <- views, ns /= want]
    soas = [soa | (_, Serving _ soa) <- views]
    evidence = fmap describe views
    describe (s, Unreachable why) = s <> ": unreachable (" <> why <> ")"
    describe (s, NotServing why) = s <> ": " <> why
    describe (s, Serving ns soa) =
        s <> ": NS " <> list ns <> maybe ", no SOA" (\(m, serial) -> ", SOA " <> m <> " serial " <> serial) soa

-------------------------------------------------------------------------------
-- Names

-- | A name to look up, and optionally the values it should have.
data RecordQuery = RecordQuery
    { rqType :: Text
    , rqName :: Text
    , rqExpected :: [Text]
    }
    deriving (Eq, Show)

{- | Reads @[TYPE:]NAME[=VALUE[,VALUE]...]@; the type defaults to @A@. Values
are split on commas, so a TXT value holding one cannot be declared.
-}
parseRecordQuery :: Text -> Either Text RecordQuery
parseRecordQuery t
    | Text.null name = Left ("no name in " <> t)
    | otherwise = Right (RecordQuery ty (normalizeName name) (sort (nub (normalizeValue ty <$> values))))
  where
    (lhs, rhs) = Text.breakOn "=" (Text.strip t)
    values = filter (not . Text.null) (Text.strip <$> Text.splitOn "," (Text.drop 1 rhs))
    (ty, name) = case Text.breakOn ":" lhs of
        (p, rest)
            | not (Text.null rest), not (Text.null p), Text.all isAlpha p -> (Text.map toUpper p, Text.strip (Text.drop 1 rest))
        _ -> ("A", Text.strip lhs)

-- | One record's data for comparison: names without their dot, TXT without its quotes.
normalizeValue :: Text -> Text -> Text
normalizeValue ty v
    | ty `elem` ["NS", "CNAME", "PTR"] = normalizeName v
    | ty == "TXT" = Text.dropAround (== '"') (Text.replace "\" \"" "" (Text.strip v))
    | ty == "AAAA" = Text.toLower (Text.strip v)
    | otherwise = Text.strip v

{- | The values the zone's server and the resolver give for a query, made
comparable: the records of the asked type when the zone holds some, and
otherwise the CNAME at the name itself on both sides, since a zone that
hands out an alias to a name outside itself says nothing more, while a
resolver follows it.
-}
answerValues :: RecordQuery -> DigAnswer -> DigAnswer -> ([Text], [Text])
answerValues q zone resolver = case ofType (rqType q) zone of
    [] -> (alias zone, alias resolver)
    vs -> (vs, ofType (rqType q) resolver)
  where
    ofType ty ans = sort (nub [normalizeValue ty (recData r) | r <- digRecords ans, recType r == ty])
    alias ans = sort (nub [normalizeName (recData r) | r <- digRecords ans, recType r == "CNAME", recOwner r == rqName q])

{- | Tells a wrong record from one that has not propagated. The zone's values
are 'Nothing' when none of its servers answered for it, the resolver's when
it did not answer.
-}
judgeRecord :: RecordQuery -> Maybe [Text] -> Maybe [Text] -> Judgement
judgeRecord q zone resolver = case (zone, resolver) of
    (Nothing, _) -> Judgement Nothing (["none of the zone's servers answered for it"] <> seen) Nothing
    (Just zs, _)
        | null zs ->
            Judgement (Just False) evidence (Just "Wrong record: the zone itself holds no such record, so there is nothing to propagate.")
        | not (null (rqExpected q)) && zs /= rqExpected q ->
            Judgement (Just False) evidence (Just "Wrong record: the zone itself does not hold the declared values, so waiting will not help.")
    (Just _, Nothing) -> Judgement Nothing evidence Nothing
    (Just zs, Just rs)
        | zs == rs -> Judgement (Just True) evidence Nothing
        | otherwise ->
            Judgement (Just False) evidence (Just "Not propagated: the zone holds the record and the resolver does not answer with it. Either an older answer is cached until its TTL runs out, or the domain is not delegated to these servers (see the delegation finding).")
  where
    evidence =
        ["the zone's servers say: " <> maybe "(no answer)" list zone]
            <> ["declared: " <> list (rqExpected q) | not (null (rqExpected q))]
            <> seen
    seen = ["the resolver says: " <> maybe "(no answer)" list resolver]
