# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

from types import TracebackType
from typing import Optional, Sequence, Type

from glean.glass.clients import GlassService
from glean.glass.thrift_types import (
    ClientInfo,
    DocumentSymbolIndex,
    DocumentSymbolListXResult,
    DocumentSymbolsRequest,
    FileIncludeLocationRequest,
    FileIncludeLocationResults,
    LocationRange,
    RelatedNeighborhoodRequest,
    RelatedNeighborhoodResult,
    RequestOptions,
    ResolveSymbolsRequest,
    ResolveSymbolsResult,
    SearchRelatedRequest,
    SearchRelatedResult,
    SymbolDescription,
    SymbolLocation,
    SymbolSearchRequest,
    SymbolSearchResult,
    USRSymbolDefinition,
    USRToDefinitionRequest,
)
from libfb.py.build_info import BuildInfo
from libfb.py.pwdutils import get_current_user_name
from libfb.py.thrift_client_factory import get_client

# See for explanation https://fburl.com/code/57snxyzh
try:
    from servicerouter.py3 import ClientParams, get_sr_client
except ImportError:
    get_sr_client = None
    ClientParams = None


# Client info for all Glass API calls
client_info: ClientInfo = ClientInfo(
    name="api-python3",
    unixname=get_current_user_name(),
    application=BuildInfo.get_build_rule(),
)


def _with_client_info(options: Optional[RequestOptions] = None) -> RequestOptions:
    """
    Creates or updates RequestOptions with client_info attached.
    """
    if options is None:
        return RequestOptions(client_info=client_info)

    return RequestOptions(
        revision=options.revision,
        limit=options.limit,
        feature_flags=options.feature_flags,
        strict=options.strict,
        exact_revision=options.exact_revision,
        matching_revision=options.matching_revision,
        content_check=options.content_check,
        attribute_opts=options.attribute_opts,
        client_info=client_info,
    )


class GlassClient:
    """
    A high level client wrapper for the Glass service.

    This wrapper automatically attaches client_info to all requests

    Typical usage:

        from glean.glass.client.py3 import GlassClient

        async def main() -> None:
            async with await GlassClient.new() as client:
                result = await client.searchSymbol(request)
                symbol = await client.describeSymbol(symbol_id)

    """

    def __init__(self, service: object) -> None:
        """
        Initialize the GlassClient.

        Args:
            service: GlassService context manager from get_client/get_sr_client.
        """
        self._service_context = service
        self._service: object = None

    @staticmethod
    async def new(
        tier: str = "glean.glass",
        host: Optional[str] = None,
        port: Optional[int] = None,
        params: Optional[object] = None,
        client_id: Optional[str] = None,
    ) -> "GlassClient":
        """
        Create a new GlassClient connected to the specified tier.

        This is the recommended way to create a GlassClient. It creates
        the underlying GlassService connection internally and manages
        its lifecycle.

        Args:
            tier: The service tier to connect to. Defaults to "glean.glass".
            host: Optional host to connect to directly (bypasses service router).
            port: Optional port to connect to directly (requires host).
            params: Optional ClientParams for custom connection configuration.
                If provided, takes precedence over host/port.
            client_id: Optional ServiceRouter client ID. If provided, sets the
                client ID on the ServiceRouter params.

        Returns:
            A GlassClient that can be used as an async context manager.

        Example:
            async with await GlassClient.new() as client:
                result = await client.searchSymbol(request)

            # With custom host/port:
            async with await GlassClient.new(host="127.0.0.1", port=8080) as client:
                result = await client.searchSymbol(request)

            # With custom params:
            from servicerouter.py3 import ClientParams
            params = ClientParams().setSingleHost(ipAddr="127.0.0.1", port=8080)
            async with await GlassClient.new(params=params) as client:
                result = await client.searchSymbol(request)
        """
        if (host is None) != (port is None):
            raise ValueError(
                "Both host and port must be provided together, or neither."
            )

        if params is not None and (host is not None or port is not None):
            raise ValueError(
                "Cannot specify both params and host/port. Use one or the other."
            )

        if get_sr_client is not None:
            if params is None:
                params = ClientParams()

            if client_id is not None:
                params.setClientId(client_id)

            if host is not None and port is not None:
                params.setSingleHost(ipAddr=host, port=port)

            service_context = get_sr_client(
                GlassService,
                tier=tier,
                params=params,
            )
        else:
            service_context = get_client(GlassService, tier)

        return GlassClient(service_context)

    async def __aenter__(self) -> "GlassClient":
        self._service = await self._service_context.__aenter__()
        return self

    async def __aexit__(
        self,
        exc_type: Optional[Type[BaseException]],
        exc: Optional[BaseException],
        tb: Optional[TracebackType],
    ) -> None:
        await self._service_context.__aexit__(exc_type, exc, tb)

    async def documentSymbolListX(
        self,
        request: DocumentSymbolsRequest,
        options: Optional[RequestOptions] = None,
    ) -> DocumentSymbolListXResult:
        """Get document symbols with extended information."""
        return await self._service.documentSymbolListX(
            request, _with_client_info(options)
        )

    async def documentSymbolIndex(
        self,
        request: DocumentSymbolsRequest,
        options: Optional[RequestOptions] = None,
    ) -> DocumentSymbolIndex:
        """Get document symbol index."""
        return await self._service.documentSymbolIndex(
            request, _with_client_info(options)
        )

    async def findReferenceRanges(
        self,
        symbol: str,
        options: Optional[RequestOptions] = None,
    ) -> Sequence[LocationRange]:
        """Find reference ranges for a symbol."""
        return await self._service.findReferenceRanges(
            symbol, _with_client_info(options)
        )

    async def describeSymbol(
        self,
        symbol: str,
        options: Optional[RequestOptions] = None,
    ) -> SymbolDescription:
        """Describe a symbol."""
        return await self._service.describeSymbol(symbol, _with_client_info(options))

    async def symbolLocation(
        self,
        symbol: str,
        options: Optional[RequestOptions] = None,
    ) -> SymbolLocation:
        """Get symbol location."""
        return await self._service.symbolLocation(symbol, _with_client_info(options))

    async def resolveSymbols(
        self,
        request: ResolveSymbolsRequest,
        options: Optional[RequestOptions] = None,
    ) -> ResolveSymbolsResult:
        """Resolve symbols."""
        return await self._service.resolveSymbols(request, _with_client_info(options))

    async def searchSymbol(
        self,
        request: SymbolSearchRequest,
        options: Optional[RequestOptions] = None,
    ) -> SymbolSearchResult:
        """Search for symbols."""
        return await self._service.searchSymbol(request, _with_client_info(options))

    async def searchRelated(
        self,
        symbol: str,
        request: SearchRelatedRequest,
        options: Optional[RequestOptions] = None,
    ) -> SearchRelatedResult:
        """Search for related symbols."""
        return await self._service.searchRelated(
            symbol, _with_client_info(options), request
        )

    async def searchRelatedNeighborhood(
        self,
        symbol: str,
        request: RelatedNeighborhoodRequest,
        options: Optional[RequestOptions] = None,
    ) -> RelatedNeighborhoodResult:
        """Search for related neighborhood."""
        return await self._service.searchRelatedNeighborhood(
            symbol, _with_client_info(options), request
        )

    async def fileIncludeLocations(
        self,
        request: FileIncludeLocationRequest,
        options: Optional[RequestOptions] = None,
    ) -> FileIncludeLocationResults:
        """Get file include locations."""
        return await self._service.fileIncludeLocations(
            request, _with_client_info(options)
        )

    async def clangUSRToDefinition(
        self,
        hash: str,
        options: Optional[RequestOptions] = None,
    ) -> USRSymbolDefinition:
        """Get clang USR to definition."""
        return await self._service.clangUSRToDefinition(
            hash, _with_client_info(options)
        )

    async def usrToDefinition(
        self,
        request: USRToDefinitionRequest,
        options: Optional[RequestOptions] = None,
    ) -> USRSymbolDefinition:
        """Get USR to definition."""
        return await self._service.usrToDefinition(request, _with_client_info(options))
