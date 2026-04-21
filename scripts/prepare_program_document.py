#!/usr/bin/env python3
import argparse
import csv
import mimetypes
import os
import sys
import urllib.parse
from pathlib import Path

import requests


def parse_args():
    parser = argparse.ArgumentParser(description="Download and convert an official program document.")
    parser.add_argument("--project-dir", default=".")
    parser.add_argument("--document-id", required=True)
    parser.add_argument("--source-id", required=True)
    parser.add_argument("--candidate-id", required=True)
    parser.add_argument("--document-role", default="programa-base")
    parser.add_argument("--official-page-url", default="")
    parser.add_argument("--download-url", required=True)
    parser.add_argument("--source-name", required=True)
    parser.add_argument("--title", required=True)
    parser.add_argument("--published-at", required=True)
    parser.add_argument("--discovery-method", default="manual_curated")
    parser.add_argument("--is-primary", action="store_true")
    parser.add_argument("--notes", default="")
    return parser.parse_args()


def detect_extension(url, content_type, content_disposition="", payload=b""):
    parsed = urllib.parse.urlparse(url)
    suffix = Path(parsed.path).suffix.lower()
    if suffix:
      return suffix

    if content_disposition:
        for part in content_disposition.split(";"):
            part = part.strip()
            if part.lower().startswith("filename="):
                filename = part.split("=", 1)[1].strip().strip("\"'")
                disposition_suffix = Path(filename).suffix.lower()
                if disposition_suffix:
                    return disposition_suffix

    if payload.startswith(b"%PDF-"):
        return ".pdf"

    guessed = mimetypes.guess_extension((content_type or "").split(";")[0].strip())
    if guessed:
      return guessed

    return ".bin"


def convert_to_markdown(source_path):
    try:
        from markitdown import MarkItDown
    except ImportError as exc:
        raise RuntimeError(
            "The Python package 'markitdown' is required. Install it locally before running this script."
        ) from exc

    converter = MarkItDown()
    result = converter.convert(str(source_path))
    return result.text_content


def write_registry_row(registry_path, row):
    existing_rows = []
    if registry_path.exists():
        with registry_path.open(newline="", encoding="utf-8") as handle:
            existing_rows = list(csv.DictReader(handle))

    updated = []
    replaced = False
    for existing in existing_rows:
        if existing.get("document_id") == row["document_id"]:
            updated.append(row)
            replaced = True
        else:
            updated.append(existing)

    if not replaced:
        updated.append(row)

    fieldnames = [
        "document_id",
        "source_id",
        "candidate_id",
        "document_role",
        "is_primary",
        "official_page_url",
        "download_url",
        "source_name",
        "title",
        "published_at",
        "discovery_method",
        "download_status",
        "conversion_status",
        "pdf_path",
        "markdown_path",
        "notes",
    ]

    registry_path.parent.mkdir(parents=True, exist_ok=True)
    with registry_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(updated)


def main():
    args = parse_args()
    project_dir = Path(args.project_dir).resolve()
    registry_path = project_dir / "data" / "program_documents" / "program_documents.csv"
    candidate_dir = project_dir / "data" / "program_documents" / "files" / args.candidate_id
    candidate_dir.mkdir(parents=True, exist_ok=True)

    response = requests.get(
        args.download_url,
        headers={"User-Agent": "Codex program document fetcher"},
        timeout=120
    )
    response.raise_for_status()

    payload = response.content
    content_type = response.headers.get("Content-Type", "")
    content_disposition = response.headers.get("Content-Disposition", "")
    extension = detect_extension(args.download_url, content_type, content_disposition, payload)

    local_source_path = candidate_dir / f"{args.document_id}{extension}"
    local_source_path.write_bytes(payload)

    markdown_text = convert_to_markdown(local_source_path)
    markdown_path = candidate_dir / f"{args.document_id}.md"
    markdown_path.write_text(markdown_text, encoding="utf-8")

    row = {
        "document_id": args.document_id,
        "source_id": args.source_id,
        "candidate_id": args.candidate_id,
        "document_role": args.document_role,
        "is_primary": "TRUE" if args.is_primary else "FALSE",
        "official_page_url": args.official_page_url,
        "download_url": args.download_url,
        "source_name": args.source_name,
        "title": args.title,
        "published_at": args.published_at,
        "discovery_method": args.discovery_method,
        "download_status": "downloaded",
        "conversion_status": "converted",
        "pdf_path": os.path.relpath(local_source_path, project_dir).replace(os.sep, "/") if local_source_path.suffix.lower() == ".pdf" else "",
        "markdown_path": os.path.relpath(markdown_path, project_dir).replace(os.sep, "/"),
        "notes": args.notes,
    }

    write_registry_row(registry_path, row)
    print(f"Prepared program document {args.document_id}")


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:  # pragma: no cover
        print(f"error: {exc}", file=sys.stderr)
        sys.exit(1)
