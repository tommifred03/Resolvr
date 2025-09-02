;; DisputeSettlementTemplates Contract
;; Provides reusable dispute resolution templates and automated settlement proposals
;; Complements the main Resolvr arbitration system

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_TEMPLATE_NOT_FOUND (err u201))
(define-constant ERR_INVALID_CATEGORY (err u202))
(define-constant ERR_TEMPLATE_EXISTS (err u203))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u204))
(define-constant ERR_DISPUTE_NOT_FOUND (err u205))
(define-constant ERR_INVALID_PERCENTAGE (err u206))
(define-constant ERR_TEMPLATE_DISABLED (err u207))
(define-constant ERR_PROPOSAL_EXPIRED (err u208))
(define-constant ERR_ALREADY_ACCEPTED (err u209))

(define-data-var template-counter uint u0)
(define-data-var proposal-counter uint u0)
(define-data-var proposal-validity-period uint u72)

;; Settlement template storage
(define-map settlement-templates
  uint
  {
    name: (string-ascii 100),
    category: (string-ascii 30),
    description: (string-ascii 500),
    plaintiff-percentage: uint,
    defendant-percentage: uint,
    conditions: (string-ascii 300),
    created-by: principal,
    created-at: uint,
    usage-count: uint,
    success-rate: uint,
    active: bool
  }
)

;; Template categories for organization
(define-map template-categories
  (string-ascii 30)
  {
    category-id: uint,
    total-templates: uint,
    active-templates: uint,
    description: (string-ascii 200)
  }
)

;; Automated settlement proposals
(define-map settlement-proposals
  uint
  {
    dispute-id: uint,
    template-id: uint,
    proposed-by: principal,
    plaintiff-amount: uint,
    defendant-amount: uint,
    proposal-terms: (string-ascii 500),
    created-at: uint,
    expires-at: uint,
    plaintiff-accepted: bool,
    defendant-accepted: bool,
    status: (string-ascii 20)
  }
)

;; Template usage statistics
(define-map template-usage-stats
  { template-id: uint, dispute-category: (string-ascii 30) }
  {
    times-used: uint,
    successful-resolutions: uint,
    average-resolution-time: uint,
    last-used: uint
  }
)

;; Dispute-specific template recommendations
(define-map dispute-template-matches
  { dispute-id: uint, template-id: uint }
  {
    match-score: uint,
    recommended-by: principal,
    recommendation-reason: (string-ascii 200)
  }
)

;; Create a new settlement template
(define-public (create-settlement-template 
    (name (string-ascii 100)) 
    (category (string-ascii 30)) 
    (description (string-ascii 500))
    (plaintiff-percentage uint)
    (defendant-percentage uint)
    (conditions (string-ascii 300)))
  (let (
    (template-id (+ (var-get template-counter) u1))
    (creator tx-sender)
  )
    (asserts! (is-eq creator CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (+ plaintiff-percentage defendant-percentage) u100) ERR_INVALID_PERCENTAGE)
    (asserts! (or (is-eq category "financial") (is-eq category "commercial") (is-eq category "technical") (is-eq category "general")) ERR_INVALID_CATEGORY)
    
    (map-set settlement-templates template-id {
      name: name,
      category: category,
      description: description,
      plaintiff-percentage: plaintiff-percentage,
      defendant-percentage: defendant-percentage,
      conditions: conditions,
      created-by: creator,
      created-at: stacks-block-height,
      usage-count: u0,
      success-rate: u0,
      active: true
    })
    
    ;; Update category statistics
    (let (
      (current-category (default-to { category-id: u0, total-templates: u0, active-templates: u0, description: "" } 
                                   (map-get? template-categories category)))
    )
      (map-set template-categories category (merge current-category {
        total-templates: (+ (get total-templates current-category) u1),
        active-templates: (+ (get active-templates current-category) u1)
      }))
    )
    
    (var-set template-counter template-id)
    (ok template-id)
  )
)

;; Generate automated settlement proposal
(define-public (generate-settlement-proposal (dispute-id uint) (template-id uint) (total-amount uint))
  (let (
    (template (unwrap! (map-get? settlement-templates template-id) ERR_TEMPLATE_NOT_FOUND))
    (proposer tx-sender)
    (proposal-id (+ (var-get proposal-counter) u1))
    (current-block stacks-block-height)
    (expiry-block (+ current-block (var-get proposal-validity-period)))
  )
    (asserts! (get active template) ERR_TEMPLATE_DISABLED)
    
    (let (
      (plaintiff-amount (/ (* total-amount (get plaintiff-percentage template)) u100))
      (defendant-amount (/ (* total-amount (get defendant-percentage template)) u100))
    )
      (map-set settlement-proposals proposal-id {
        dispute-id: dispute-id,
        template-id: template-id,
        proposed-by: proposer,
        plaintiff-amount: plaintiff-amount,
        defendant-amount: defendant-amount,
        proposal-terms: (get conditions template),
        created-at: current-block,
        expires-at: expiry-block,
        plaintiff-accepted: false,
        defendant-accepted: false,
        status: "pending"
      })
      
      ;; Update template usage
      (map-set settlement-templates template-id (merge template {
        usage-count: (+ (get usage-count template) u1)
      }))
      
      (var-set proposal-counter proposal-id)
      (ok proposal-id)
    )
  )
)

;; Accept a settlement proposal
(define-public (accept-settlement-proposal (proposal-id uint) (dispute-id uint))
  (let (
    (proposal (unwrap! (map-get? settlement-proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
    (acceptor tx-sender)
    (current-block stacks-block-height)
  )
    (asserts! (is-eq (get dispute-id proposal) dispute-id) ERR_DISPUTE_NOT_FOUND)
    (asserts! (< current-block (get expires-at proposal)) ERR_PROPOSAL_EXPIRED)
    (asserts! (is-eq (get status proposal) "pending") ERR_ALREADY_ACCEPTED)
    
    ;; Check if acceptor is a party to the dispute (this would need integration with main contract)
    ;; For now, we'll allow any principal to accept
    
    (let (
      (is-plaintiff-accepting (and (not (get plaintiff-accepted proposal)) true)) ;; Simplified logic
      (is-defendant-accepting (and (not (get defendant-accepted proposal)) true)) ;; Simplified logic
      (new-plaintiff-accepted (if is-plaintiff-accepting true (get plaintiff-accepted proposal)))
      (new-defendant-accepted (if is-defendant-accepting true (get defendant-accepted proposal)))
      (new-status (if (and new-plaintiff-accepted new-defendant-accepted) "accepted" "partial"))
    )
      (map-set settlement-proposals proposal-id (merge proposal {
        plaintiff-accepted: new-plaintiff-accepted,
        defendant-accepted: new-defendant-accepted,
        status: new-status
      }))
      
      ;; If both parties accepted, update template success rate
      (if (is-eq new-status "accepted")
        (let (
          (template (unwrap! (map-get? settlement-templates (get template-id proposal)) ERR_TEMPLATE_NOT_FOUND))
          (current-success-rate (get success-rate template))
          (usage-count (get usage-count template))
          (new-success-rate (if (> usage-count u0) (/ (+ (* current-success-rate usage-count) u100) usage-count) u100))
        )
          (map-set settlement-templates (get template-id proposal) (merge template {
            success-rate: new-success-rate
          }))
        )
        true
      )
      
      (ok new-status)
    )
  )
)

;; Recommend template for a dispute
(define-public (recommend-template (dispute-id uint) (template-id uint) (reason (string-ascii 200)))
  (let (
    (template (unwrap! (map-get? settlement-templates template-id) ERR_TEMPLATE_NOT_FOUND))
    (recommender tx-sender)
  )
    (asserts! (get active template) ERR_TEMPLATE_DISABLED)
    
    ;; Calculate basic match score (simplified)
    (let (
      (base-score u50)
      (usage-bonus (if (> (get usage-count template) u10) u20 u0))
      (success-bonus (/ (get success-rate template) u5))
      (match-score (+ base-score usage-bonus success-bonus))
    )
      (map-set dispute-template-matches { dispute-id: dispute-id, template-id: template-id } {
        match-score: match-score,
        recommended-by: recommender,
        recommendation-reason: reason
      })
      (ok match-score)
    )
  )
)

;; Toggle template active status
(define-public (toggle-template-status (template-id uint))
  (let (
    (template (unwrap! (map-get? settlement-templates template-id) ERR_TEMPLATE_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    
    (map-set settlement-templates template-id (merge template {
      active: (not (get active template))
    }))
    
    ;; Update category statistics
    (let (
      (category (get category template))
      (current-category (unwrap! (map-get? template-categories category) ERR_INVALID_CATEGORY))
      (active-change (if (get active template) -1 1))
    )
      (map-set template-categories category (merge current-category {
        active-templates: (+ (get active-templates current-category) (if (get active template) u0 u1))
      }))
    )
    
    (ok (not (get active template)))
  )
)

;; Update proposal validity period
(define-public (update-proposal-validity-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set proposal-validity-period new-period)
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-settlement-template (template-id uint))
  (map-get? settlement-templates template-id)
)

(define-read-only (get-template-category (category (string-ascii 30)))
  (map-get? template-categories category)
)

(define-read-only (get-settlement-proposal (proposal-id uint))
  (map-get? settlement-proposals proposal-id)
)

(define-read-only (get-template-usage-stats (template-id uint) (dispute-category (string-ascii 30)))
  (map-get? template-usage-stats { template-id: template-id, dispute-category: dispute-category })
)

(define-read-only (get-dispute-template-recommendation (dispute-id uint) (template-id uint))
  (map-get? dispute-template-matches { dispute-id: dispute-id, template-id: template-id })
)

(define-read-only (get-current-template-id)
  (var-get template-counter)
)

(define-read-only (get-current-proposal-id)
  (var-get proposal-counter)
)

(define-read-only (get-proposal-validity-period)
  (var-get proposal-validity-period)
)

(define-read-only (is-proposal-valid (proposal-id uint))
  (match (map-get? settlement-proposals proposal-id)
    proposal (and 
               (is-eq (get status proposal) "pending")
               (< stacks-block-height (get expires-at proposal)))
    false
  )
)

(define-read-only (get-template-effectiveness (template-id uint))
  (match (map-get? settlement-templates template-id)
    template (let (
      (usage-count (get usage-count template))
      (success-rate (get success-rate template))
    )
      (if (> usage-count u0)
        {
          usage-count: usage-count,
          success-rate: success-rate,
          effectiveness-score: (/ (* usage-count success-rate) u100)
        }
        {
          usage-count: u0,
          success-rate: u0,
          effectiveness-score: u0
        }
      )
    )
    {
      usage-count: u0,
      success-rate: u0,
      effectiveness-score: u0
    }
  )
)

(define-read-only (list-active-templates-by-category (category (string-ascii 30)))
  (get-template-category category)
)