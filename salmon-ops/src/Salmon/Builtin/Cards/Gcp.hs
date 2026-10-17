{- | Web UI cards for the GCP builtins ("Salmon.Builtin.Nodes.Gcp"), in the
style of "Salmon.Builtin.Cards" (feature d8a79380), and like those
__opt-in__: a recipe attaches one per node with 'Salmon.Builtin.Cards.withCard'.

> withCard (GcpCards.instance_ inst) (Compute.gceInstance r gcloud inst)

Each card takes the builtin's own declaration value and shows only what the
builtin already makes public in its @ref@, @help@ and @notes@: a project, a
zone or region, a name, a machine type, a DNS name and its records. It never
shows instance metadata (a startup script may carry anything), a secret's
value, or any digest of either.

Each card also links to the resource's page in the Cloud console, built from
that same public data (project, zone or region, name), percent-encoded. __The
links have not been followed against a real project__: they are the console's
documented URL shapes at the time of writing, and where a resource has no
stable page of its own (an address, a record set, a regional balancer) the
link goes to the list it is on.
-}
module Salmon.Builtin.Cards.Gcp (
    -- * Cards
    instance_,
    address,
    firewallRule,
    subnet,
    managedZone,
    recordSet,
    bucket,
    secret,
    secretFile,
    applicationLoadBalancer,

    -- * Before rendering
    instanceCard,
    addressCard,
    firewallRuleCard,
    subnetCard,
    managedZoneCard,
    recordSetCard,
    bucketCard,
    secretCard,
    secretFileCard,
    applicationLoadBalancerCard,

    -- * Console links
    consoleUrl,
) where

import Data.Text (Text)
import qualified Data.Text as Text

import Salmon.Builtin.Cards (Card (..), Link (..), Verdicts (..), percentEncode, renderCard)
import qualified Salmon.Builtin.Nodes.Gcp.CloudDns as Dns
import qualified Salmon.Builtin.Nodes.Gcp.Compute as Compute
import Salmon.Builtin.Nodes.Gcp.Core (Project (..), Region (..), Zone (..))
import qualified Salmon.Builtin.Nodes.Gcp.LoadBalancing as Lb
import qualified Salmon.Builtin.Nodes.Gcp.SecretManager as Secrets
import qualified Salmon.Builtin.Nodes.Gcp.Storage as Storage
import Salmon.Builtin.Nodes.SecretDelivery (Placement (..))

{- | A Cloud console page of this project: the path segments, each
percent-encoded, then @?project=@. Only public data goes in.
-}
consoleUrl :: [Text] -> Project -> Text
consoleUrl segments p =
    "https://console.cloud.google.com/"
        <> Text.intercalate "/" (fmap percentEncode segments)
        <> "?project="
        <> percentEncode p.projectId

console :: Text -> [Text] -> Project -> [Link]
console label segments p = [Link label (consoleUrl segments p)]

unchecked :: Text
unchecked = "no check result on this page yet"

noCheck :: Text
noCheck = "the node declares no check"

-- | For 'Compute.gceInstance'. The metadata (and its files) are not shown.
instance_ :: Compute.Instance -> Text
instance_ = renderCard . instanceCard

-- | 'instance_' before rendering.
instanceCard :: Compute.Instance -> Card
instanceCard i =
    Card
        { cardFacts =
            [ ("instance", i.instanceName)
            , ("project", i.instanceProject.projectId)
            , ("zone", i.instanceZone.zoneName)
            , ("machine type", Compute.renderMachineType i.instanceMachineType)
            , ("declared power", power i.instancePower)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "there, in the declared power state, machine type and scopes"
                , verdictToApply = "absent or not as declared: up creates, starts, stops or resizes it, as permitted (the reason says what differs)"
                , verdictUnsure = "could not tell the instance's state"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "creates the instance if absent, then starts, resumes or stops it to the declared power; a different machine type is changed only when the declaration permits a stop"
        , cardDown = "deletes the instance if it is there, whatever its state"
        , cardLinks = console "instance in the Cloud console" ["compute", "instancesDetail", "zones", i.instanceZone.zoneName, "instances", i.instanceName] i.instanceProject
        }
  where
    power Compute.PoweredOn = "on"
    power Compute.PoweredOff = "off"

-- | For 'Compute.address'.
address :: Compute.Address -> Text
address = renderCard . addressCard

-- | 'address' before rendering.
addressCard :: Compute.Address -> Card
addressCard a =
    Card
        { cardFacts =
            [ ("address", a.addressName)
            , ("project", a.addressProject.projectId)
            , ("region", a.addressRegion.regionName)
            ]
                <> kind a.addressKind
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "reserved, as declared"
                , verdictToApply = "not reserved, or holding another address than the declared one: up reserves it"
                , verdictUnsure = "could not tell whether the address is reserved"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "reserves the address (retrying while a freshly enabled API settles)"
        , cardDown = "releases the address if it is reserved"
        , cardLinks = console "IP addresses in the Cloud console" ["networking", "addresses", "list"] a.addressProject
        }
  where
    kind Compute.ExternalAddress = [("kind", "external")]
    kind (Compute.InternalAddress sub lit) = [("kind", "internal"), ("subnet", sub)] <> [("literal", l) | Just l <- [lit]]

-- | For 'Compute.firewallRule'.
firewallRule :: Compute.FirewallRule -> Text
firewallRule = renderCard . firewallRuleCard

-- | 'firewallRule' before rendering.
firewallRuleCard :: Compute.FirewallRule -> Card
firewallRuleCard fw =
    Card
        { cardFacts =
            [ ("rule", fw.firewallName)
            , ("project", fw.firewallProject.projectId)
            , ("network", fw.firewallNetwork)
            , ("allows", fw.firewallAllow)
            , ("from", commas fw.firewallSourceRanges)
            , ("to tags", commas fw.firewallTargetTags)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the rule is there"
                , verdictToApply = "absent: up creates it"
                , verdictUnsure = "could not tell whether the rule exists"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "creates the ingress rule (an existing rule of that name is not compared or changed)"
        , cardDown = "deletes the rule if it is there"
        , cardLinks = console "rule in the Cloud console" ["networking", "firewalls", "details", fw.firewallName] fw.firewallProject
        }

-- | For 'Compute.subnet'.
subnet :: Compute.Subnet -> Text
subnet = renderCard . subnetCard

-- | 'subnet' before rendering.
subnetCard :: Compute.Subnet -> Card
subnetCard n =
    Card
        { cardFacts =
            [ ("subnet", n.subnetName)
            , ("project", n.subnetProject.projectId)
            , ("region", n.subnetRegion.regionName)
            , ("network", n.subnetNetwork)
            , ("range", n.subnetRange)
            , ("purpose", Compute.renderSubnetPurpose n.subnetPurpose)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the subnet exists, with the declared purpose"
                , verdictToApply = "absent (up creates it), or of another purpose (up does not change that; the reason names both)"
                , verdictUnsure = "could not tell whether the subnet exists"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "creates the subnetwork (the range is not compared with an existing one)"
        , cardDown = "deletes the subnetwork if it is there"
        , cardLinks = console "subnet in the Cloud console" ["networking", "subnetworks", "details", n.subnetRegion.regionName, n.subnetName] n.subnetProject
        }

-- | For 'Dns.managedZone'.
managedZone :: Dns.ManagedZone -> Text
managedZone = renderCard . managedZoneCard

-- | 'managedZone' before rendering.
managedZoneCard :: Dns.ManagedZone -> Card
managedZoneCard z =
    Card
        { cardFacts =
            [ ("zone", z.zoneName)
            , ("project", z.zoneProject.projectId)
            , ("serves", Dns.fqdn z.zoneDnsName)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the zone exists and serves the declared domain"
                , verdictToApply = "absent, or a zone of that name serves another domain (then up fails rather than take it over)"
                , verdictUnsure = "could not tell whether the zone exists"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "creates the public managed zone"
        , cardDown = "deletes the zone if it is there and serves the declared domain; Cloud DNS refuses while records other than the apex NS and SOA remain"
        , cardLinks = zoneLink z
        }

zoneLink :: Dns.ManagedZone -> [Link]
zoneLink z = console "zone in the Cloud console" ["net-services", "dns", "zones", z.zoneName, "details"] z.zoneProject

{- | For 'Dns.recordSet' (and 'Dns.resolvedRecordSet', whose declared data
the card shows as "resolved when applied" since the record's data is not
known at graph build: pass 'True' for that one).
-}
recordSet :: Bool -> Dns.RecordSet -> Text
recordSet resolved = renderCard . recordSetCard resolved

-- | 'recordSet' before rendering.
recordSetCard :: Bool -> Dns.RecordSet -> Card
recordSetCard resolved rs =
    Card
        { cardFacts =
            [ ("name", Dns.fqdn rs.recordName)
            , ("type", Dns.renderRecordType rs.recordType)
            , ("ttl", Text.pack (show rs.recordTtl) <> "s")
            , ("data", if resolved then "resolved when applied" else Text.intercalate " " rs.recordData)
            , ("zone", rs.recordZone.zoneName)
            , ("project", rs.recordZone.zoneProject.projectId)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "holds exactly the declared TTL and data"
                , verdictToApply = "absent, or holding other data or TTL: up writes the declared set"
                , verdictUnsure = "could not tell what the record set holds (or its data is not readable yet)"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "creates the record set, or updates it in place: the name and type are the effect site, so one made by hand is taken over"
        , cardDown = "deletes the record set at that name and type if there is one, whatever it holds"
        , cardLinks = zoneLink rs.recordZone
        }

-- | For 'Storage.bucket'.
bucket :: Storage.Bucket -> Text
bucket = renderCard . bucketCard

-- | 'bucket' before rendering.
bucketCard :: Storage.Bucket -> Card
bucketCard b =
    Card
        { cardFacts =
            [ ("bucket", b.bucketName)
            , ("project", b.bucketProject.projectId)
            , ("location", b.bucketLocation.regionName)
            , ("uniform access", if b.bucketUniformBucketLevelAccess then "on" else "off")
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "the bucket is there"
                , verdictToApply = "absent: up creates it"
                , verdictUnsure = "could not tell whether the bucket exists"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "creates the bucket"
        , cardDown = "deletes the bucket if it is there"
        , cardLinks = console "bucket in the Cloud console" ["storage", "browser", b.bucketName] b.bucketProject
        }

-- | For 'Secrets.secret': the secret's name, never a version's value.
secret :: Secrets.Secret -> Text
secret = renderCard . secretCard

-- | 'secret' before rendering.
secretCard :: Secrets.Secret -> Card
secretCard sec =
    Card
        { cardFacts =
            [ ("secret", sec.secretName)
            , ("project", sec.secretProject.projectId)
            , ("replication", sec.secretReplication)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "present"
                , verdictToApply = "missing: up creates it (empty: versions are their own nodes)"
                , verdictUnsure = "could not tell whether the secret exists"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "creates the secret, with no version"
        , cardDown = "deletes the secret if it is there, and with it every version"
        , cardLinks = console "secret in the Cloud console" ["security", "secret-manager", "secret", sec.secretName, "versions"] sec.secretProject
        }

{- | For 'Secrets.secretFile': where the secret is written and from which
secret, and whether it is there. Never the bytes, and never a digest.
-}
secretFile :: Secrets.SecretFile -> Text
secretFile = renderCard . secretFileCard

-- | 'secretFile' before rendering.
secretFileCard :: Secrets.SecretFile -> Card
secretFileCard f =
    Card
        { cardFacts =
            [ ("path", Text.pack place.placePath)
            , ("secret", f.fileSecretName)
            , ("version", f.fileVersion)
            , ("project", f.fileProject.projectId)
            , ("owner", place.placeOwner <> ":" <> place.placeGroup)
            , ("mode", place.placeMode)
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "present, as the secret holds it"
                , verdictToApply = "missing, different, or the secret is not readable: up writes it"
                , verdictUnsure = "could not tell whether the file is in place"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "reads the secret version as this machine's identity and writes it to the path, atomically; the bytes are never reported"
        , cardDown = "removes the file"
        , cardLinks = console "secret in the Cloud console" ["security", "secret-manager", "secret", f.fileSecretName, "versions"] f.fileProject
        }
  where
    place = f.filePlacement

-- | For 'Lb.applicationLoadBalancer' (and its @After@\/@With@ variants): the balancer's root node.
applicationLoadBalancer :: Lb.ApplicationLoadBalancer -> Text
applicationLoadBalancer = renderCard . applicationLoadBalancerCard

-- | 'applicationLoadBalancer' before rendering.
applicationLoadBalancerCard :: Lb.ApplicationLoadBalancer -> Card
applicationLoadBalancerCard alb =
    Card
        { cardFacts =
            [ ("balancer", alb.albName)
            , ("project", alb.albProject.projectId)
            , ("region", alb.albRegion.regionName)
            , ("https", if null alb.albCertificates then "no" else "yes")
            ]
        , cardVerdicts =
            Verdicts
                { verdictInPlace = "every instance-group backend reports healthy"
                , verdictToApply = "a backend is not healthy, or the declaration is invalid (the reason says which)"
                , verdictUnsure = "could not ask the balancer for its backends' health"
                , verdictNoCheck = noCheck
                , verdictUnchecked = unchecked
                }
        , cardUp = "nothing of its own: its parts (address, health check, services, URL map, certificates, proxies, forwarding rules) are its dependencies"
        , cardDown = "nothing of its own: each part's node deletes its resource"
        , cardLinks = console "load balancers in the Cloud console" ["net-services", "loadbalancing", "list", "loadBalancers"] alb.albProject
        }

commas :: [Text] -> Text
commas [] = "(none)"
commas xs = Text.intercalate "," xs
