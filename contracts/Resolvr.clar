(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_DISPUTE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_NOT_ARBITRATOR (err u105))
(define-constant ERR_VOTING_ENDED (err u106))
(define-constant ERR_DISPUTE_RESOLVED (err u107))
(define-constant ERR_INVALID_PARTY (err u108))

(define-data-var dispute-counter uint u0)
(define-data-var arbitrator-fee uint u1000000)
(define-data-var voting-period uint u144)

(define-map disputes
  uint
  {
    plaintiff: principal,
    defendant: principal,
    amount: uint,
    description: (string-ascii 500),
    status: (string-ascii 20),
    created-at: uint,
    voting-ends: uint,
    votes-for-plaintiff: uint,
    votes-for-defendant: uint,
    total-arbitrators: uint,
    resolved-at: (optional uint)
  }
)

(define-map arbitrators
  principal
  {
    reputation: uint,
    total-cases: uint,
    active: bool,
    stake: uint
  }
)

(define-map dispute-votes
  { dispute-id: uint, arbitrator: principal }
  { vote: (string-ascii 10), timestamp: uint }
)

(define-map dispute-funds
  uint
  uint
)

(define-public (register-arbitrator (stake-amount uint))
  (let ((arbitrator tx-sender))
    (asserts! (>= stake-amount u5000000) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? stake-amount arbitrator (as-contract tx-sender)))
    (map-set arbitrators arbitrator {
      reputation: u100,
      total-cases: u0,
      active: true,
      stake: stake-amount
    })
    (ok true)
  )
)

(define-public (create-dispute (defendant principal) (amount uint) (description (string-ascii 500)))
  (let (
    (dispute-id (+ (var-get dispute-counter) u1))
    (plaintiff tx-sender)
    (current-block stacks-block-height)
  )
    (asserts! (not (is-eq plaintiff defendant)) ERR_INVALID_PARTY)
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? amount plaintiff (as-contract tx-sender)))
    (map-set disputes dispute-id {
      plaintiff: plaintiff,
      defendant: defendant,
      amount: amount,
      description: description,
      status: "pending",
      created-at: current-block,
      voting-ends: (+ current-block (var-get voting-period)),
      votes-for-plaintiff: u0,
      votes-for-defendant: u0,
      total-arbitrators: u0,
      resolved-at: none
    })
    (map-set dispute-funds dispute-id amount)
    (var-set dispute-counter dispute-id)
    (ok dispute-id)
  )
)

(define-public (respond-to-dispute (dispute-id uint) (counter-stake uint))
  (let (
    (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
    (defendant tx-sender)
  )
    (asserts! (is-eq defendant (get defendant dispute)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) "pending") ERR_INVALID_STATUS)
    (asserts! (>= counter-stake (get amount dispute)) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? counter-stake defendant (as-contract tx-sender)))
    (map-set disputes dispute-id (merge dispute { status: "active" }))
    (map-set dispute-funds dispute-id (+ (get amount dispute) counter-stake))
    (ok true)
  )
)

(define-public (vote-on-dispute (dispute-id uint) (vote-for (string-ascii 10)))
  (let (
    (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
    (arbitrator tx-sender)
    (arbitrator-info (unwrap! (map-get? arbitrators arbitrator) ERR_NOT_ARBITRATOR))
    (current-block stacks-block-height)
  )
    (asserts! (get active arbitrator-info) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status dispute) "active") ERR_INVALID_STATUS)
    (asserts! (<= current-block (get voting-ends dispute)) ERR_VOTING_ENDED)
    (asserts! (is-none (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: arbitrator })) ERR_ALREADY_VOTED)
    (asserts! (or (is-eq vote-for "plaintiff") (is-eq vote-for "defendant")) ERR_INVALID_PARTY)
    
    (map-set dispute-votes { dispute-id: dispute-id, arbitrator: arbitrator } {
      vote: vote-for,
      timestamp: current-block
    })
    
    (let (
      (new-plaintiff-votes (if (is-eq vote-for "plaintiff") 
                             (+ (get votes-for-plaintiff dispute) u1) 
                             (get votes-for-plaintiff dispute)))
      (new-defendant-votes (if (is-eq vote-for "defendant") 
                             (+ (get votes-for-defendant dispute) u1) 
                             (get votes-for-defendant dispute)))
      (new-total-arbitrators (+ (get total-arbitrators dispute) u1))
    )
      (map-set disputes dispute-id (merge dispute {
        votes-for-plaintiff: new-plaintiff-votes,
        votes-for-defendant: new-defendant-votes,
        total-arbitrators: new-total-arbitrators
      }))
      
      (map-set arbitrators arbitrator (merge arbitrator-info {
        total-cases: (+ (get total-cases arbitrator-info) u1),
        reputation: (+ (get reputation arbitrator-info) u10)
      }))
    )
    (ok true)
  )
)

(define-public (resolve-dispute (dispute-id uint))
  (let (
    (dispute (unwrap! (map-get? disputes dispute-id) ERR_DISPUTE_NOT_FOUND))
    (current-block stacks-block-height)
    (total-funds (unwrap! (map-get? dispute-funds dispute-id) ERR_DISPUTE_NOT_FOUND))
  )
    (asserts! (is-eq (get status dispute) "active") ERR_INVALID_STATUS)
    (asserts! (or (> current-block (get voting-ends dispute)) (>= (get total-arbitrators dispute) u3)) ERR_VOTING_ENDED)
    
    (let (
      (plaintiff-votes (get votes-for-plaintiff dispute))
      (defendant-votes (get votes-for-defendant dispute))
      (winner (if (> plaintiff-votes defendant-votes) "plaintiff" "defendant"))
      (winner-address (if (> plaintiff-votes defendant-votes) 
                        (get plaintiff dispute) 
                        (get defendant dispute)))
    )
      (try! (as-contract (stx-transfer? total-funds tx-sender winner-address)))
      (map-set disputes dispute-id (merge dispute {
        status: "resolved",
        resolved-at: (some current-block)
      }))
      (map-delete dispute-funds dispute-id)
      (ok winner)
    )
  )
)

(define-public (withdraw-arbitrator-stake)
  (let (
    (arbitrator tx-sender)
    (arbitrator-info (unwrap! (map-get? arbitrators arbitrator) ERR_NOT_ARBITRATOR))
  )
    (asserts! (get active arbitrator-info) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get stake arbitrator-info) tx-sender arbitrator)))
    (map-set arbitrators arbitrator (merge arbitrator-info { active: false, stake: u0 }))
    (ok true)
  )
)

(define-public (update-arbitrator-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set arbitrator-fee new-fee)
    (ok true)
  )
)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes dispute-id)
)

(define-read-only (get-arbitrator (arbitrator principal))
  (map-get? arbitrators arbitrator)
)

(define-read-only (get-dispute-vote (dispute-id uint) (arbitrator principal))
  (map-get? dispute-votes { dispute-id: dispute-id, arbitrator: arbitrator })
)

(define-read-only (get-dispute-funds (dispute-id uint))
  (map-get? dispute-funds dispute-id)
)

(define-read-only (get-current-dispute-id)
  (var-get dispute-counter)
)

(define-read-only (get-arbitrator-fee)
  (var-get arbitrator-fee)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (is-dispute-active (dispute-id uint))
  (match (map-get? disputes dispute-id)
    dispute (is-eq (get status dispute) "active")
    false
  )
)

(define-read-only (can-resolve-dispute (dispute-id uint))
  (match (map-get? disputes dispute-id)
    dispute (and 
              (is-eq (get status dispute) "active")
              (or 
                (> stacks-block-height (get voting-ends dispute))
                (>= (get total-arbitrators dispute) u3)
              )
            )
    false
  )
)