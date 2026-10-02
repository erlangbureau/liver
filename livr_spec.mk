# LIVR specification sync helpers.
#
# Local cases live in tests/cases/livr (Erlang terms).
# Upstream source: https://github.com/koorchik/LIVR
#
# Import/check tools are Erlang modules under scripts/.
# Import uses OTP stdlib json (OTP 27+) with order-preserving proplists.

LIVR_SPEC_REF ?= master
LIVR_SPEC_TMP ?= $(CURDIR)/.tmp/LIVR
LIVR_SPEC_CASES ?= $(CURDIR)/tests/cases/livr
LIVR_SPEC_ZIP ?= $(CURDIR)/.tmp/livr.zip
LIVR_SPEC_EBIN ?= $(CURDIR)/.tmp/livr_spec_ebin

.PHONY: livr-spec-fetch livr-spec-tools livr-spec-import livr-spec-check

livr-spec-fetch:
	$(verbose) mkdir -p $(dir $(LIVR_SPEC_ZIP))
	$(verbose) curl -fsSL -o $(LIVR_SPEC_ZIP) \
		https://codeload.github.com/koorchik/LIVR/zip/$(LIVR_SPEC_REF)
	$(verbose) rm -rf $(LIVR_SPEC_TMP)
	$(verbose) mkdir -p $(dir $(LIVR_SPEC_TMP))
	$(verbose) unzip -q $(LIVR_SPEC_ZIP) -d $(dir $(LIVR_SPEC_TMP))
	$(verbose) EXTRACTED=$$(find $(dir $(LIVR_SPEC_TMP)) -mindepth 1 -maxdepth 1 -type d -name 'LIVR-*' | head -1); \
		mv "$$EXTRACTED" $(LIVR_SPEC_TMP)

livr-spec-tools:
	$(verbose) mkdir -p $(LIVR_SPEC_EBIN)
	$(verbose) erlc -o $(LIVR_SPEC_EBIN) scripts/livr_import.erl scripts/livr_spec_check.erl

# GitHub zip archive comment stores the commit SHA (unzip -z).
livr-spec-import: livr-spec-fetch livr-spec-tools
	$(verbose) SHA=$$(unzip -z $(LIVR_SPEC_ZIP) 2>/dev/null | sed -n '2p' | tr -d '\r'); \
		if [ -z "$$SHA" ]; then echo "Could not detect LIVR SHA from zip comment" >&2; exit 1; fi; \
		erl -noshell -pa $(LIVR_SPEC_EBIN) \
			-eval "ok = livr_import:run(\"$(LIVR_SPEC_TMP)\", \"$$SHA\", \"$(LIVR_SPEC_CASES)\"), halt()."

livr-spec-check: livr-spec-fetch livr-spec-tools
	$(verbose) erl -noshell -pa $(LIVR_SPEC_EBIN) \
		-eval "ok = livr_spec_check:run(\"$(LIVR_SPEC_TMP)\", \"$(LIVR_SPEC_CASES)/MANIFEST\"), halt()."
