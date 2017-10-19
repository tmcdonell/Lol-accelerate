
#define mkDispatch(ty)                                    \
; instance Dispatch (ty) where                            \
;   expose'   = $( runQ (_expose   (Proxy::Proxy (ty))) ) \
;   unexpose' = $( runQ (_unexpose (Proxy::Proxy (ty))) ) \
;                                                         \
;   eq        = $( runQ (_eq  (Proxy::Proxy (ty))) )      \
;   neq       = $( runQ (_neq (Proxy::Proxy (ty))) )      \
;                                                         \
;   pL'       = $( runQ (_pL    (Proxy::Proxy (ty))) )    \
;   pLInv'    = $( runQ (_pLInv (Proxy::Proxy (ty))) )    \
;   pGPow'    = $( runQ (_pGPow (Proxy::Proxy (ty))) )    \
;   pGDec'    = $( runQ (_pGDec (Proxy::Proxy (ty))) )    \
;                                                         \
;   embedPow' = $( runQ (_embedPow (Proxy::Proxy (ty))) ) \

/* ;   pGInvPow' = $( runQ (_pGInvPow (Proxy::Proxy (ty))) ) \ */
/* ;   pGInvDec' = $( runQ (_pGInvDec (Proxy::Proxy (ty))) ) \ */
/* ;   divCheck' = $( runQ (_divCheck (Proxy::Proxy (ty))) ) \ */

