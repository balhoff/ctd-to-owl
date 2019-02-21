ctdcams: CTD_chem_gene_ixns_structured.xml
	rm -rf ctdcams && mkdir ctdcams && ctd-to-owl

ctd.jnl: ctdcams
	blazegraph-runner load --informat=turtle --journal=ctd.jnl --graph='http://ctdbase.org/' ctdcams &&\
	blazegraph-runner load --informat=turtle --journal=ctd.jnl --graph='http://ctdbase.org/vocab/' vocab

geneToChem.tsv: geneToChem.rq ctd.jnl
	blazegraph-runner select --journal=ctd.jnl --outformat=tsv geneToChem.rq geneToChem.tsv

chemToGene.tsv: chemToGene.rq ctd.jnl
	blazegraph-runner select --journal=ctd.jnl --outformat=tsv chemToGene.rq chemToGene.tsv

ctd.tsv: chemToGene.tsv geneToChem.tsv
	cp chemToGene.tsv ctd.tsv && cat geneToChem.tsv >>ctd.tsv

ctd-grouped.tsv: ctd.tsv
	datamash --sort groupby 1,2,3,4,5,6,7,8 unique 9 <ctd.tsv >ctd-grouped.tsv
