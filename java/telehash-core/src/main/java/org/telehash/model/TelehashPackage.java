/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package org.telehash.model;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.json.model.JsonPackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see org.telehash.model.TelehashFactory
 * @model kind="package"
 * @generated
 */
public interface TelehashPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "telehash";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://telehash.org/ecore/2010";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "telehash";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	TelehashPackage eINSTANCE = org.telehash.model.impl.TelehashPackageImpl
			.init();

	/**
	 * The meta object id for the '{@link org.telehash.model.impl.TelexImpl <em>Telex</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.telehash.model.impl.TelexImpl
	 * @see org.telehash.model.impl.TelehashPackageImpl#getTelex()
	 * @generated
	 */
	int TELEX = 0;

	/**
	 * The feature id for the '<em><b>Unmatched</b></em>' map.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__UNMATCHED = JsonPackage.JS_OBJECT__UNMATCHED;

	/**
	 * The feature id for the '<em><b>To</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__TO = JsonPackage.JS_OBJECT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>End</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__END = JsonPackage.JS_OBJECT_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Line</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__LINE = JsonPackage.JS_OBJECT_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Ring</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__RING = JsonPackage.JS_OBJECT_FEATURE_COUNT + 3;

	/**
	 * The feature id for the '<em><b>Bytes Received</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__BYTES_RECEIVED = JsonPackage.JS_OBJECT_FEATURE_COUNT + 4;

	/**
	 * The feature id for the '<em><b>See</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__SEE = JsonPackage.JS_OBJECT_FEATURE_COUNT + 5;

	/**
	 * The feature id for the '<em><b>Tap</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX__TAP = JsonPackage.JS_OBJECT_FEATURE_COUNT + 6;

	/**
	 * The number of structural features of the '<em>Telex</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TELEX_FEATURE_COUNT = JsonPackage.JS_OBJECT_FEATURE_COUNT + 7;

	/**
	 * The meta object id for the '{@link org.telehash.model.impl.TapRuleImpl <em>Tap Rule</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.telehash.model.impl.TapRuleImpl
	 * @see org.telehash.model.impl.TelehashPackageImpl#getTapRule()
	 * @generated
	 */
	int TAP_RULE = 1;

	/**
	 * The feature id for the '<em><b>Unmatched</b></em>' map.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAP_RULE__UNMATCHED = JsonPackage.JS_OBJECT__UNMATCHED;

	/**
	 * The feature id for the '<em><b>Is</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAP_RULE__IS = JsonPackage.JS_OBJECT_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Has</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAP_RULE__HAS = JsonPackage.JS_OBJECT_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Tap Rule</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int TAP_RULE_FEATURE_COUNT = JsonPackage.JS_OBJECT_FEATURE_COUNT + 2;

	/**
	 * The meta object id for the '{@link org.telehash.model.impl.LineImpl <em>Line</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.telehash.model.impl.LineImpl
	 * @see org.telehash.model.impl.TelehashPackageImpl#getLine()
	 * @generated
	 */
	int LINE = 2;

	/**
	 * The feature id for the '<em><b>Address</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__ADDRESS = 0;

	/**
	 * The feature id for the '<em><b>End</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__END = 1;

	/**
	 * The feature id for the '<em><b>Neighbors</b></em>' attribute list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__NEIGHBORS = 2;

	/**
	 * The feature id for the '<em><b>Ring In</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__RING_IN = 3;

	/**
	 * The feature id for the '<em><b>Ring Out</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__RING_OUT = 4;

	/**
	 * The feature id for the '<em><b>Init</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__INIT = 5;

	/**
	 * The feature id for the '<em><b>Seen At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__SEEN_AT = 6;

	/**
	 * The feature id for the '<em><b>Sent At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__SENT_AT = 7;

	/**
	 * The feature id for the '<em><b>Line At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__LINE_AT = 8;

	/**
	 * The feature id for the '<em><b>Tap Last At</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__TAP_LAST_AT = 9;

	/**
	 * The feature id for the '<em><b>Br</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__BR = 10;

	/**
	 * The feature id for the '<em><b>Br In</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__BR_IN = 11;

	/**
	 * The feature id for the '<em><b>Br Out</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__BR_OUT = 12;

	/**
	 * The feature id for the '<em><b>Bsent</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__BSENT = 13;

	/**
	 * The feature id for the '<em><b>Line Id</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__LINE_ID = 14;

	/**
	 * The feature id for the '<em><b>Visible</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__VISIBLE = 15;

	/**
	 * The feature id for the '<em><b>Advertised</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__ADVERTISED = 16;

	/**
	 * The feature id for the '<em><b>Rules</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__RULES = 17;

	/**
	 * The feature id for the '<em><b>Session</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE__SESSION = 18;

	/**
	 * The number of structural features of the '<em>Line</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int LINE_FEATURE_COUNT = 19;

	/**
	 * The meta object id for the '<em>Endpoint</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see java.net.InetSocketAddress
	 * @see org.telehash.model.impl.TelehashPackageImpl#getEndpoint()
	 * @generated
	 */
	int ENDPOINT = 3;

	/**
	 * The meta object id for the '<em>Hash</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.telehash.Hash
	 * @see org.telehash.model.impl.TelehashPackageImpl#getHash()
	 * @generated
	 */
	int HASH = 4;

	/**
	 * The meta object id for the '<em>Io Session</em>' data type.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.apache.mina.core.session.IoSession
	 * @see org.telehash.model.impl.TelehashPackageImpl#getIoSession()
	 * @generated
	 */
	int IO_SESSION = 5;

	/**
	 * Returns the meta object for class '{@link org.telehash.model.Telex <em>Telex</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Telex</em>'.
	 * @see org.telehash.model.Telex
	 * @generated
	 */
	EClass getTelex();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getTo <em>To</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>To</em>'.
	 * @see org.telehash.model.Telex#getTo()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_To();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getEnd <em>End</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>End</em>'.
	 * @see org.telehash.model.Telex#getEnd()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_End();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getLine <em>Line</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Line</em>'.
	 * @see org.telehash.model.Telex#getLine()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_Line();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getRing <em>Ring</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ring</em>'.
	 * @see org.telehash.model.Telex#getRing()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_Ring();

	/**
	 * Returns the meta object for the attribute list '{@link org.telehash.model.Telex#getSee <em>See</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>See</em>'.
	 * @see org.telehash.model.Telex#getSee()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_See();

	/**
	 * Returns the meta object for the containment reference list '{@link org.telehash.model.Telex#getTap <em>Tap</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Tap</em>'.
	 * @see org.telehash.model.Telex#getTap()
	 * @see #getTelex()
	 * @generated
	 */
	EReference getTelex_Tap();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Telex#getBytesReceived <em>Bytes Received</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Bytes Received</em>'.
	 * @see org.telehash.model.Telex#getBytesReceived()
	 * @see #getTelex()
	 * @generated
	 */
	EAttribute getTelex_BytesReceived();

	/**
	 * Returns the meta object for class '{@link org.telehash.model.TapRule <em>Tap Rule</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Tap Rule</em>'.
	 * @see org.telehash.model.TapRule
	 * @generated
	 */
	EClass getTapRule();

	/**
	 * Returns the meta object for the containment reference '{@link org.telehash.model.TapRule#getIs <em>Is</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference '<em>Is</em>'.
	 * @see org.telehash.model.TapRule#getIs()
	 * @see #getTapRule()
	 * @generated
	 */
	EReference getTapRule_Is();

	/**
	 * Returns the meta object for the attribute list '{@link org.telehash.model.TapRule#getHas <em>Has</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Has</em>'.
	 * @see org.telehash.model.TapRule#getHas()
	 * @see #getTapRule()
	 * @generated
	 */
	EAttribute getTapRule_Has();

	/**
	 * Returns the meta object for class '{@link org.telehash.model.Line <em>Line</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Line</em>'.
	 * @see org.telehash.model.Line
	 * @generated
	 */
	EClass getLine();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getAddress <em>Address</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Address</em>'.
	 * @see org.telehash.model.Line#getAddress()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Address();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getEnd <em>End</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>End</em>'.
	 * @see org.telehash.model.Line#getEnd()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_End();

	/**
	 * Returns the meta object for the attribute list '{@link org.telehash.model.Line#getNeighbors <em>Neighbors</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute list '<em>Neighbors</em>'.
	 * @see org.telehash.model.Line#getNeighbors()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Neighbors();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getRingIn <em>Ring In</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ring In</em>'.
	 * @see org.telehash.model.Line#getRingIn()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_RingIn();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getRingOut <em>Ring Out</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Ring Out</em>'.
	 * @see org.telehash.model.Line#getRingOut()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_RingOut();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getInit <em>Init</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Init</em>'.
	 * @see org.telehash.model.Line#getInit()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Init();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getSeenAt <em>Seen At</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Seen At</em>'.
	 * @see org.telehash.model.Line#getSeenAt()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_SeenAt();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getSentAt <em>Sent At</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Sent At</em>'.
	 * @see org.telehash.model.Line#getSentAt()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_SentAt();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getLineAt <em>Line At</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Line At</em>'.
	 * @see org.telehash.model.Line#getLineAt()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_LineAt();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getTapLastAt <em>Tap Last At</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Tap Last At</em>'.
	 * @see org.telehash.model.Line#getTapLastAt()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_TapLastAt();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getBr <em>Br</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Br</em>'.
	 * @see org.telehash.model.Line#getBr()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Br();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getBrIn <em>Br In</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Br In</em>'.
	 * @see org.telehash.model.Line#getBrIn()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_BrIn();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getBrOut <em>Br Out</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Br Out</em>'.
	 * @see org.telehash.model.Line#getBrOut()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_BrOut();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getBsent <em>Bsent</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Bsent</em>'.
	 * @see org.telehash.model.Line#getBsent()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Bsent();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getLineId <em>Line Id</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Line Id</em>'.
	 * @see org.telehash.model.Line#getLineId()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_LineId();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#isVisible <em>Visible</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Visible</em>'.
	 * @see org.telehash.model.Line#isVisible()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Visible();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#isAdvertised <em>Advertised</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Advertised</em>'.
	 * @see org.telehash.model.Line#isAdvertised()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Advertised();

	/**
	 * Returns the meta object for the containment reference list '{@link org.telehash.model.Line#getRules <em>Rules</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Rules</em>'.
	 * @see org.telehash.model.Line#getRules()
	 * @see #getLine()
	 * @generated
	 */
	EReference getLine_Rules();

	/**
	 * Returns the meta object for the attribute '{@link org.telehash.model.Line#getSession <em>Session</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Session</em>'.
	 * @see org.telehash.model.Line#getSession()
	 * @see #getLine()
	 * @generated
	 */
	EAttribute getLine_Session();

	/**
	 * Returns the meta object for data type '{@link java.net.InetSocketAddress <em>Endpoint</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Endpoint</em>'.
	 * @see java.net.InetSocketAddress
	 * @model instanceClass="java.net.InetSocketAddress"
	 * @generated
	 */
	EDataType getEndpoint();

	/**
	 * Returns the meta object for data type '{@link org.telehash.Hash <em>Hash</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Hash</em>'.
	 * @see org.telehash.Hash
	 * @model instanceClass="org.telehash.Hash"
	 * @generated
	 */
	EDataType getHash();

	/**
	 * Returns the meta object for data type '{@link org.apache.mina.core.session.IoSession <em>Io Session</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for data type '<em>Io Session</em>'.
	 * @see org.apache.mina.core.session.IoSession
	 * @model instanceClass="org.apache.mina.core.session.IoSession" serializeable="false"
	 * @generated
	 */
	EDataType getIoSession();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	TelehashFactory getTelehashFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link org.telehash.model.impl.TelexImpl <em>Telex</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.telehash.model.impl.TelexImpl
		 * @see org.telehash.model.impl.TelehashPackageImpl#getTelex()
		 * @generated
		 */
		EClass TELEX = eINSTANCE.getTelex();

		/**
		 * The meta object literal for the '<em><b>To</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__TO = eINSTANCE.getTelex_To();

		/**
		 * The meta object literal for the '<em><b>End</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__END = eINSTANCE.getTelex_End();

		/**
		 * The meta object literal for the '<em><b>Line</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__LINE = eINSTANCE.getTelex_Line();

		/**
		 * The meta object literal for the '<em><b>Ring</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__RING = eINSTANCE.getTelex_Ring();

		/**
		 * The meta object literal for the '<em><b>See</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__SEE = eINSTANCE.getTelex_See();

		/**
		 * The meta object literal for the '<em><b>Tap</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TELEX__TAP = eINSTANCE.getTelex_Tap();

		/**
		 * The meta object literal for the '<em><b>Bytes Received</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TELEX__BYTES_RECEIVED = eINSTANCE.getTelex_BytesReceived();

		/**
		 * The meta object literal for the '{@link org.telehash.model.impl.TapRuleImpl <em>Tap Rule</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.telehash.model.impl.TapRuleImpl
		 * @see org.telehash.model.impl.TelehashPackageImpl#getTapRule()
		 * @generated
		 */
		EClass TAP_RULE = eINSTANCE.getTapRule();

		/**
		 * The meta object literal for the '<em><b>Is</b></em>' containment reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference TAP_RULE__IS = eINSTANCE.getTapRule_Is();

		/**
		 * The meta object literal for the '<em><b>Has</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute TAP_RULE__HAS = eINSTANCE.getTapRule_Has();

		/**
		 * The meta object literal for the '{@link org.telehash.model.impl.LineImpl <em>Line</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.telehash.model.impl.LineImpl
		 * @see org.telehash.model.impl.TelehashPackageImpl#getLine()
		 * @generated
		 */
		EClass LINE = eINSTANCE.getLine();

		/**
		 * The meta object literal for the '<em><b>Address</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__ADDRESS = eINSTANCE.getLine_Address();

		/**
		 * The meta object literal for the '<em><b>End</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__END = eINSTANCE.getLine_End();

		/**
		 * The meta object literal for the '<em><b>Neighbors</b></em>' attribute list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__NEIGHBORS = eINSTANCE.getLine_Neighbors();

		/**
		 * The meta object literal for the '<em><b>Ring In</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__RING_IN = eINSTANCE.getLine_RingIn();

		/**
		 * The meta object literal for the '<em><b>Ring Out</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__RING_OUT = eINSTANCE.getLine_RingOut();

		/**
		 * The meta object literal for the '<em><b>Init</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__INIT = eINSTANCE.getLine_Init();

		/**
		 * The meta object literal for the '<em><b>Seen At</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__SEEN_AT = eINSTANCE.getLine_SeenAt();

		/**
		 * The meta object literal for the '<em><b>Sent At</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__SENT_AT = eINSTANCE.getLine_SentAt();

		/**
		 * The meta object literal for the '<em><b>Line At</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__LINE_AT = eINSTANCE.getLine_LineAt();

		/**
		 * The meta object literal for the '<em><b>Tap Last At</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__TAP_LAST_AT = eINSTANCE.getLine_TapLastAt();

		/**
		 * The meta object literal for the '<em><b>Br</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__BR = eINSTANCE.getLine_Br();

		/**
		 * The meta object literal for the '<em><b>Br In</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__BR_IN = eINSTANCE.getLine_BrIn();

		/**
		 * The meta object literal for the '<em><b>Br Out</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__BR_OUT = eINSTANCE.getLine_BrOut();

		/**
		 * The meta object literal for the '<em><b>Bsent</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__BSENT = eINSTANCE.getLine_Bsent();

		/**
		 * The meta object literal for the '<em><b>Line Id</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__LINE_ID = eINSTANCE.getLine_LineId();

		/**
		 * The meta object literal for the '<em><b>Visible</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__VISIBLE = eINSTANCE.getLine_Visible();

		/**
		 * The meta object literal for the '<em><b>Advertised</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__ADVERTISED = eINSTANCE.getLine_Advertised();

		/**
		 * The meta object literal for the '<em><b>Rules</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference LINE__RULES = eINSTANCE.getLine_Rules();

		/**
		 * The meta object literal for the '<em><b>Session</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute LINE__SESSION = eINSTANCE.getLine_Session();

		/**
		 * The meta object literal for the '<em>Endpoint</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see java.net.InetSocketAddress
		 * @see org.telehash.model.impl.TelehashPackageImpl#getEndpoint()
		 * @generated
		 */
		EDataType ENDPOINT = eINSTANCE.getEndpoint();

		/**
		 * The meta object literal for the '<em>Hash</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.telehash.Hash
		 * @see org.telehash.model.impl.TelehashPackageImpl#getHash()
		 * @generated
		 */
		EDataType HASH = eINSTANCE.getHash();

		/**
		 * The meta object literal for the '<em>Io Session</em>' data type.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see org.apache.mina.core.session.IoSession
		 * @see org.telehash.model.impl.TelehashPackageImpl#getIoSession()
		 * @generated
		 */
		EDataType IO_SESSION = eINSTANCE.getIoSession();

	}

} //TelehashPackage
