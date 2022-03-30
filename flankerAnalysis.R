# Flanker
# analysis of CSE in a the flanker task replication.


library(ggplot2)


flankplot <- ggplot(data=Flanker_KP2, mapping = aes(y=Reaction.Time, colour = Nminus1))

flankplot+
  geom_boxplot(mapping = aes(x=Nminus1, colour = Type))+
  facet_wrap(~lang)

flankcurve <- ggplot(data=Flanker_KP2) + 
  geom_density(aes(Reaction.Time, colour = interaction(Type, Nminus1)))+
  facet_wrap(~lang)

flanksummary <- 
Flanker_KP2 %>% as_tibble() %>% 
  group_by(Participant.Private.ID, Type, Nminus1) %>% 
  summarise(meanRT=mean(Reaction.Time))